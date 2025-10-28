from pathlib import Path
import re
import math
import datetime as dt
import pandas as pd

# Configuracoes fixas
XLSX_PATH = Path(__file__).with_name("video.xlsx")
SHEET = 0          # indice ou nome da planilha
STEP_SECONDS = 5.0 # passo de tempo (em segundos)


_EXCEL_ESC_RE = re.compile(r"_x([0-9A-Fa-f]{4})_")


def clean_text(value) -> str:
    """Remove sequências especiais do Excel (_xNNNN_),
    caracteres de controle e normaliza espaços.
    """
    if value is None or (hasattr(pd, "isna") and pd.isna(value)):
        return ""
    s = str(value)

    def _unescape(m: re.Match) -> str:
        code = int(m.group(1), 16)
        ch = chr(code)
        # Converte controles em espaço para evitar colagem de palavras
        if code < 32 or code == 127:
            return " "
        return ch

    # Desfaz padrões _xNNNN_ (pode haver mais de um)
    prev = None
    while prev != s:
        prev = s
        s = _EXCEL_ESC_RE.sub(_unescape, s)

    # Remove/normaliza caracteres de controle restantes
    s = re.sub(r"[\x00-\x1F\x7F]", " ", s)
    # Normaliza qualquer sequência de espaço em um único espaço
    s = re.sub(r"\s+", " ", s).strip()
    return s


def parse_duration_to_seconds(value) -> float:
    """Parse estrito em formato HH:MM:SS.

    - Aceita: strings exatamente "HH:MM:SS" (HH pode ter 1-2 digitos) ou
      objetos de tempo (datetime/time/Timestamp), sempre convertidos para HH:MM:SS.
    - Rejeita: numeros, fracoes de dia, formatos "MM:SS", milissegundos e quaisquer outros.
    - Vazios/NaN viram 0.0.
    """

    # Vazio/NaN
    if value is None or (hasattr(pd, "isna") and pd.isna(value)):
        return 0.0

    # Se vier como objeto temporal, converte para HH:MM:SS
    if isinstance(value, pd.Timestamp):
        t = value.to_pydatetime()
        s = f"{t.hour:02d}:{t.minute:02d}:{t.second:02d}"
    elif isinstance(value, dt.datetime):
        s = f"{value.hour:02d}:{value.minute:02d}:{value.second:02d}"
    elif isinstance(value, dt.time):
        s = f"{value.hour:02d}:{value.minute:02d}:{value.second:02d}"
    else:
        s = str(value).strip()

    # Somente HH:MM:SS estrito
    m = re.fullmatch(r"(\d{1,2}):(\d{2}):(\d{2})", s)
    if not m:
        raise ValueError(f"Duracao invalida: '{s}'. Esperado HH:MM:SS")

    h = int(m.group(1))
    mi = int(m.group(2))
    se = int(m.group(3))
    if mi >= 60 or se >= 60:
        raise ValueError(f"Duracao invalida: '{s}'. Minutos/segundos devem ser < 60")
    return h * 3600 + mi * 60 + se


def ts_hhmmss_mmm(seconds_float: float) -> str:
    total_ms = int(round(seconds_float * 1000))
    if total_ms < 0:
        total_ms = 0
    hours = total_ms // 3_600_000
    rem = total_ms % 3_600_000
    minutes = rem // 60_000
    rem = rem % 60_000
    seconds = rem // 1000
    ms = rem % 1000
    return f"{hours:02d}:{minutes:02d}:{seconds:02d},{ms:03d}"


def make_srt(text, total_seconds: float, step_seconds: float = 5.0) -> str:
    # Tratar NaN/None como vazio
    if text is None or (hasattr(pd, "isna") and pd.isna(text)):
        text = ""
    else:
        text = clean_text(text)

    if total_seconds <= 0:
        return ""

    # Tokeniza por espaco e distribui palavras entre os intervalos
    words = [w for w in text.split() if w]
    W = len(words)
    # Numero de slots de tempo baseado no passo
    n_slots = max(1, int(math.ceil(total_seconds / float(step_seconds))))

    # Para evitar repeticao e segmentos vazios, limitar n ao numero de palavras
    if W == 0:
        return ""
    n = min(n_slots, W)

    # Construcao usando divisao inteira para repartir equitativamente
    out_lines = []
    for i in range(n):
        # fatia de palavras [ws:we)
        ws = (W * i) // n
        we = (W * (i + 1)) // n
        chunk_words = words[ws:we]
        if not chunk_words:
            # Garantia: com n<=W isso nao deve ocorrer
            continue

        # tempo proporcional (n segmentos ao longo de total_seconds)
        start = (total_seconds * i) / n
        end = (total_seconds * (i + 1)) / n
        if end <= start:
            end = start + 0.001

        idx = i + 1
        out_lines.append(
            f"{idx}\n{ts_hhmmss_mmm(start)} --> {ts_hhmmss_mmm(end)}\n{' '.join(chunk_words)}\n"
        )

    return "\n".join(out_lines)


def main():
    # Usa a primeira linha como cabeçalho da planilha
    df = pd.read_excel(XLSX_PATH, sheet_name=SHEET, header=0)

    # Garante que existam pelo menos 3 colunas
    while df.shape[1] < 3:
        df[f"__empty{df.shape[1]}"] = ""

    # Trabalha sempre com as 3 primeiras colunas (ordem da planilha)
    cols = list(df.columns[:3])

    out_dir = XLSX_PATH.parent
    gerados = []

    for i, row in df.iterrows():
        dur_raw = row[cols[0]]
        pt_text = row[cols[1]]
        en_text = row[cols[2]]

        def _blank(x):
            return (
                x is None
                or (hasattr(pd, "isna") and pd.isna(x))
                or (isinstance(x, str) and not x.strip())
            )

        # Pular linhas totalmente vazias
        if _blank(dur_raw) and _blank(pt_text) and _blank(en_text):
            continue

        total_seconds = parse_duration_to_seconds(dur_raw)
        row_num = f"{i+1:02d}"

        pt_name = out_dir / f"br_{row_num}.srt"
        en_name = out_dir / f"us_{row_num}.srt"

        pt_srt = make_srt(pt_text, total_seconds, STEP_SECONDS)
        en_srt = make_srt(en_text, total_seconds, STEP_SECONDS)

        pt_name.write_text(pt_srt, encoding="utf-8")
        en_name.write_text(en_srt, encoding="utf-8")

        gerados.append((pt_name.name, en_name.name, total_seconds))

    print("Arquivos gerados:")
    for pt, en, secs in gerados:
        print(f"  - {pt} | {en}   (duracao: {secs:.3f}s)")


if __name__ == "__main__":
    main()

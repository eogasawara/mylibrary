"""
Generate SRT subtitle files from a simple Excel worksheet.

Input expectations (subtitle_segments.xlsx):
- Column 1: segment duration in strict HH:MM:SS (e.g., 00:00:05)
- Column 2: Portuguese text for the segment
- Column 3: English text for the segment

For each row, this script distributes the words evenly across the segment
duration and emits SRT entries with fixed time steps. Output files follow the
pattern: subtitle_pt_XX.srt and subtitle_en_XX.srt.
"""

from pathlib import Path
import re
import math
import datetime as dt
import pandas as pd

# Fixed configuration — adjust as needed
WORKBOOK_PATH = Path(__file__).with_name("subtitle_segments.xlsx")
WORKSHEET = 0            # sheet index or name
STEP_SECONDS = 5.0       # default step length inside a segment


# Excel stores certain characters using _xNNNN_ escapes — unescape them
_EXCEL_ESC_RE = re.compile(r"_x([0-9A-Fa-f]{4})_")


def clean_text(value) -> str:
    """Clean free text for subtitles.

    - Unescape Excel patterns like _x00E7_ (UTF-16 hex) where possible.
    - Remove control characters; collapse repeated whitespace.
    - Treat None/NaN as an empty string.
    """
    if value is None or (hasattr(pd, "isna") and pd.isna(value)):
        return ""
    s = str(value)

    def _unescape(m: re.Match) -> str:
        code = int(m.group(1), 16)
        ch = chr(code)
        # Map control codes to spaces to avoid word gluing
        if code < 32 or code == 127:
            return " "
        return ch

    # Repeatedly unescape until stable (handles nested/adjacent patterns)
    prev = None
    while prev != s:
        prev = s
        s = _EXCEL_ESC_RE.sub(_unescape, s)

    # Strip remaining control chars and normalize whitespace
    s = re.sub(r"[\x00-\x1F\x7F]", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def parse_duration_to_seconds(value) -> float:
    """Parse a strict HH:MM:SS duration to seconds.

    Accepts strings like 0:00:05 or 00:00:05, or time-like objects. Rejects
    numbers, MM:SS-only formats, and fractional-day representations.
    Empty/NaN values are treated as 0.0 seconds.
    """

    if value is None or (hasattr(pd, "isna") and pd.isna(value)):
        return 0.0

    # Coerce time-like objects to string HH:MM:SS
    if isinstance(value, pd.Timestamp):
        t = value.to_pydatetime()
        s = f"{t.hour:02d}:{t.minute:02d}:{t.second:02d}"
    elif isinstance(value, dt.datetime):
        s = f"{value.hour:02d}:{value.minute:02d}:{value.second:02d}"
    elif isinstance(value, dt.time):
        s = f"{value.hour:02d}:{value.minute:02d}:{value.second:02d}"
    else:
        s = str(value).strip()

    m = re.fullmatch(r"(\d{1,2}):(\d{2}):(\d{2})", s)
    if not m:
        raise ValueError(f"Invalid duration: '{s}'. Expected HH:MM:SS")

    h = int(m.group(1))
    mi = int(m.group(2))
    se = int(m.group(3))
    if mi >= 60 or se >= 60:
        raise ValueError(f"Invalid duration: '{s}'. Minutes/seconds must be < 60")
    return h * 3600 + mi * 60 + se


def srt_timestamp(seconds_float: float) -> str:
    """Format seconds as SRT timestamp HH:MM:SS,mmm"""
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


def build_srt(text: str, total_seconds: float, step_seconds: float = STEP_SECONDS) -> str:
    """Split text across the segment duration and return SRT content.

    The text is tokenized on spaces and evenly divided into N chunks, where N
    is the minimum of word count and the number of step-sized intervals inside
    the total duration. Each chunk becomes one SRT cue.
    """
    if text is None or (hasattr(pd, "isna") and pd.isna(text)):
        text = ""
    else:
        text = clean_text(text)

    if total_seconds <= 0:
        return ""

    words = [w for w in text.split() if w]
    W = len(words)
    n_slots = max(1, int(math.ceil(total_seconds / float(step_seconds))))
    if W == 0:
        return ""
    n = min(n_slots, W)

    out_lines = []
    for i in range(n):
        # Word slice [ws:we)
        ws = (W * i) // n
        we = (W * (i + 1)) // n
        chunk_words = words[ws:we]
        if not chunk_words:
            continue

        # Time slice across the segment
        start = (total_seconds * i) / n
        end = (total_seconds * (i + 1)) / n
        if end <= start:
            end = start + 0.001

        idx = i + 1
        out_lines.append(
            f"{idx}\n{srt_timestamp(start)} --> {srt_timestamp(end)}\n{' '.join(chunk_words)}\n"
        )

    return "\n".join(out_lines)


def main():
    """Read the worksheet and generate per-row PT/EN SRT files."""
    df = pd.read_excel(WORKBOOK_PATH, sheet_name=WORKSHEET, header=0)

    # Ensure at least 3 columns exist
    while df.shape[1] < 3:
        df[f"__empty{df.shape[1]}"] = ""

    # Use the first 3 columns in order: duration, pt, en
    cols = list(df.columns[:3])

    out_dir = WORKBOOK_PATH.parent
    generated = []

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

        # Skip fully empty rows
        if _blank(dur_raw) and _blank(pt_text) and _blank(en_text):
            continue

        total_seconds = parse_duration_to_seconds(dur_raw)
        row_num = f"{i+1:02d}"

        pt_name = out_dir / f"subtitle_pt_{row_num}.srt"
        en_name = out_dir / f"subtitle_en_{row_num}.srt"

        pt_srt = build_srt(pt_text, total_seconds, STEP_SECONDS)
        en_srt = build_srt(en_text, total_seconds, STEP_SECONDS)

        pt_name.write_text(pt_srt, encoding="utf-8")
        en_name.write_text(en_srt, encoding="utf-8")

        generated.append((pt_name.name, en_name.name, total_seconds))

    print("Generated files:")
    for pt, en, secs in generated:
        print(f"  - {pt} | {en}   (duration: {secs:.3f}s)")


if __name__ == "__main__":
    main()


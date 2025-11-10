Subtitle Generator

This folder contains a small, didactic tool to generate SRT subtitles from a simple Excel worksheet.

Goal
- Convert rows from an Excel plan into synchronized SRT files for Portuguese and English, splitting each line of text evenly across the specified duration.

Files
- `generate_subtitles.py` — Main script that reads the Excel worksheet and writes SRT files.
- `subtitle_segments.xlsx` — Input spreadsheet. First three columns are used:
  - Column 1: duration in HH:MM:SS (strict)
  - Column 2: Portuguese text
  - Column 3: English text
- `subtitle_pt_*.srt` — Generated Portuguese subtitles, one pair per row.
- `subtitle_en_*.srt` — Generated English subtitles, one pair per row.

How It Works
- The script tokenizes the text by spaces and distributes words evenly across time slices within the segment duration (default step: 5 seconds). Each slice becomes one SRT cue with proper timestamps.
- Excel-specific escaped characters (like `_x00E7_`) are unescaped when possible, and control characters/extra whitespace are cleaned.

Usage
- Requirements: Python 3.9+ and `pandas` (`pip install pandas openpyxl`).
- Edit `subtitle_segments.xlsx` with your content.
- Run the generator from this folder:
  - `python generate_subtitles.py`

Configuration
- Inside the script, adjust:
  - `WORKBOOK_PATH` — defaults to `subtitle_segments.xlsx` in this folder.
  - `WORKSHEET` — sheet index or name (default `0`).
  - `STEP_SECONDS` — duration per internal slice (default `5.0`).


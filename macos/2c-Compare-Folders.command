#!/bin/bash
# ============================================================================
#  BATCH compare two FOLDERS of RTF files (macOS)
#  Double-click this file. Pick the Set 1 folder, then the Set 2 folder.
#  Every .rtf file is paired by exact name, safe filename family, or strong
#  unique title/Column 1 content evidence.
#  ONE report (listing every file, including matches) is archived in the tool's
#  logs/ folder and every run is recorded in logs/audit_log.csv. Optional
#  change tables are created only for successfully compared pairs.
#  Keep the whole tool folder intact so it can find its logs/ folder.
# ============================================================================
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
RSCRIPT=""
for c in "$(command -v Rscript)" "$HOME/.local/share/micromamba/envs/rtf-comparison/bin/Rscript" \
         /usr/local/bin/Rscript /opt/homebrew/bin/Rscript \
         /Library/Frameworks/R.framework/Resources/bin/Rscript; do
  if [ -n "$c" ] && [ -x "$c" ]; then RSCRIPT="$c"; break; fi
done
if [ -z "$RSCRIPT" ]; then
  echo "Could not find R. Install it from https://cran.r-project.org and try again."
  read -n 1 -s -r -p "Press any key to close..."; echo; exit 2
fi
"$RSCRIPT" "$ROOT/R/run_compare_folder.R"
echo
read -n 1 -s -r -p "Press any key to close..."; echo

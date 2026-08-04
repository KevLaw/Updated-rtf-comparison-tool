#!/bin/bash
# ============================================================================
#  Step 1: install the R packages this tool needs (run once). (macOS)
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
"$RSCRIPT" "$ROOT/R/install_packages.R"
echo
read -n 1 -s -r -p "Press any key to close..."; echo

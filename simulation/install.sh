#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

wine msiexec /i gpss.msi

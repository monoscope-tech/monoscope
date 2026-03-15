#!/bin/bash
set -euo pipefail

REPO="monoscope-tech/monoscope"
INSTALL_DIR="${MONO_INSTALL_DIR:-$HOME/.local/bin}"
BINARY="mono"

info() { printf '\033[1;34m%s\033[0m\n' "$*"; }
error() { printf '\033[1;31merror: %s\033[0m\n' "$*" >&2; exit 1; }

detect_platform() {
  local os arch
  os="$(uname -s)"
  arch="$(uname -m)"
  case "$os" in
    Linux)  os="linux" ;;
    Darwin) os="darwin" ;;
    *)      error "Unsupported OS: $os" ;;
  esac
  case "$arch" in
    x86_64|amd64)  arch="x86_64" ;;
    arm64|aarch64) arch="arm64" ;;
    *)             error "Unsupported architecture: $arch" ;;
  esac
  echo "${os}-${arch}"
}

get_latest_version() {
  local url="https://api.github.com/repos/${REPO}/releases?per_page=20"
  curl -fsSL "$url" | grep -o '"tag_name": *"cli-v[^"]*"' | head -1 | grep -o 'cli-v[^"]*' \
    || error "Could not find a cli-v* release"
}

main() {
  local platform version archive_name download_url checksum_url tmpdir

  platform="$(detect_platform)"
  info "Detected platform: ${platform}"

  if [ -n "${1:-}" ]; then
    version="$1"
  else
    info "Fetching latest release..."
    version="$(get_latest_version)"
  fi
  info "Version: ${version}"

  archive_name="mono-${platform}.tar.gz"
  download_url="https://github.com/${REPO}/releases/download/${version}/${archive_name}"
  checksum_url="https://github.com/${REPO}/releases/download/${version}/checksums.txt"

  tmpdir="$(mktemp -d)"
  trap 'rm -rf "$tmpdir"' EXIT

  info "Downloading ${archive_name}..."
  curl -fSL --progress-bar -o "${tmpdir}/${archive_name}" "$download_url" \
    || error "Failed to download ${download_url}"

  info "Verifying checksum..."
  curl -fsSL -o "${tmpdir}/checksums.txt" "$checksum_url" \
    || error "Failed to download checksums"

  (cd "$tmpdir" && grep "$archive_name" checksums.txt | sha256sum -c --quiet 2>/dev/null) \
    || (cd "$tmpdir" && grep "$archive_name" checksums.txt | shasum -a 256 -c --quiet 2>/dev/null) \
    || error "Checksum verification failed"

  info "Extracting..."
  tar -xzf "${tmpdir}/${archive_name}" -C "$tmpdir"

  mkdir -p "$INSTALL_DIR"
  mv "${tmpdir}/${BINARY}" "${INSTALL_DIR}/${BINARY}"
  chmod +x "${INSTALL_DIR}/${BINARY}"

  info "Installed ${BINARY} to ${INSTALL_DIR}/${BINARY}"

  if ! echo "$PATH" | tr ':' '\n' | grep -qx "$INSTALL_DIR"; then
    echo ""
    info "Add ${INSTALL_DIR} to your PATH:"
    echo "  export PATH=\"${INSTALL_DIR}:\$PATH\""
    echo ""
    echo "Add this to your shell profile (~/.bashrc, ~/.zshrc, etc.)"
  fi

  echo ""
  info "Run 'mono --help' to get started"
}

main "$@"

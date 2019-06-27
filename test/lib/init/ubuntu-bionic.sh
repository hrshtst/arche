#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eEu

THIS_DIR="${1}"; shift
source "${THIS_DIR}/lib/functions.sh"
source "${THIS_DIR}/lib/init_packages.sh"

## Build tools
__init_packages_build() {
  init_packages_depends \
    'automake' \
    'autotools-dev' \
    'build-essential' \
    'cmake'
}

## VCS
__init_packages_vcs() {
  init_packages_depends \
    'git' \
    'mercurial'
}

## Fonts
__init_packages_font() {
  init_packages_depends \
    'fontforge' \
    'fonts-ipafont' \
    'powerline' \
    'ttf-mscorefonts-installer'
}

## Programming language
__init_packages_prog() {
  init_packages_depends \
    'gfortran' \
    'python3-dev' \
    'python-dev' \
    'python-virtualenv'
}

## Multimedia
__init_packages_multimedia() {
  init_packages_depends \
    'ffmpeg' \
    'imagemagick' \
    'libavcodec-extra' \
    'ubuntu-restricted-extras' \
    'vlc'
}

## Dependencies for milib
# https://www.mi.ams.eng.osaka-u.ac.jp/open-e.html
__init_packages_milib() {
  init_packages_depends \
    'freeglut3-dev' \
    'libgl1-mesa-dev' \
    'libglew-dev' \
    'libglu1-mesa-dev' \
    'libjpeg-dev' \
    'libpng-dev' \
    'libx11-dev' \
    'libxext-dev' \
    'libxpm-dev' \
    'zlib1g-dev'
}

## Utilities
__init_packages_util() {
  init_packages_depends \
    'libcanberra-gtk3-module' \
    'libcanberra-gtk-module' \
    'libncurses5-dev' \
    'libncursesw5-dev' \
    'libxml2' \
    'lv' \
    'pandoc' \
    'silversearcher-ag' \
    'vim'
}

## tmux
__init_packages_tmux() {
  init_packages_depends \
    'tmux' \
    'xclip' \
    'xsel'
  init_packages_always_config
}

__init_packages_tmux__config() {
  echo git_clone_or_update https://github.com/tmux-plugins/tpm ${HOME}/.tmux/plugins/tpm
}

## fish
__init_packages_fish__init() {
  init_packages_add_repository 'ppa:fish-shell/release-2'
}

__init_packages_fish__install() {
  init_packages_depends \
    'curl' \
    'fish' \
    'fontforge' \
    'git' \
    'unzip' \
    'wget'
}

__init_packages_fish__config() {
  markcd "${HOME}/src"
  curl -Lo ${HOME}/.config/fish/functions/fisher.fish --create-dirs https://git.io/fisher
  git clone https://github.com/powerline/fonts.git --depth=1
  cd "fonts"
  ./install.sh
  mkdir -p "${HOME}/.local/share/fonts" && cd "${HOME}/.local/share/fonts"
  curl -s https://api.github.com/repos/ryanoasis/nerd-fonts/releases/latest \
    | grep "browser_download_url.*\/FiraCode.zip" \
    | cut -d ":" -f 2,3 \
    | tr -d \" \
    | wget -qi -
  unzip FiraCode.zip
  rm FiraCode.zip
  curl -s https://api.github.com/repos/ryanoasis/nerd-fonts/releases/latest \
    | grep "browser_download_url.*\/FiraMono.zip" \
    | cut -d ":" -f 2,3 \
    | tr -d \" \
    | wget -qi -
  unzip FiraMono.zip
  rm FiraMono.zip
  fc-cache -fv
  getback
}

## Python
__init_packages_python() {
  init_packages_depends \
    'build-essential' \
    'curl' \
    'git' \
    'libbz2-dev' \
    'libffi-dev' \
    'liblzma-dev' \
    'libncurses5-dev' \
    'libncursesw5-dev' \
    'libreadline-dev' \
    'libsqlite3-dev' \
    'libssl-dev' \
    'llvm' \
    'make' \
    'python-openssl' \
    'tk-dev' \
    'wget' \
    'xz-utils' \
    'zlib1g-dev'
  init_packages_always_config
}

__init_packages_python__config() {
  echo git_clone_or_update https://github.com/pyenv/pyenv.git ${HOME}/.pyenv
}

## Go
__init_packages_go__init() {
  init_packages_add_repository 'ppa:gophers/archive'
}

__init_packages_go__install() {
  init_packages_depends 'golang-1.11-go'
}

__init_packages_go__config() {
  # Install ghq
  go get github.com/motemen/ghq
}

## Peco
__init_packages_peco() {
  init_packages_depends 'curl' 'wget'
  init_packages_always_config
}

__init_packages_peco__config() {
  binary=peco_linux_amd64.tar.gz
  if ! has peco; then
    markcd "${HOME}/usr/bin"
    curl -s https://api.github.com/repos/peco/peco/releases/latest \
      | grep "browser_download_url.*\/${binary}" \
      | cut -d ":" -f 2,3 \
      | tr -d \" \
      | wget -qi -
    tar xfz "${binary}"
    mv "${binary%.tar.gz}/peco" .
    rm -rf "${binary%.tar.gz}" "${binary}"
    getback
  fi
}

## LLVM
__init_packages_llvm__init() {
  if ! init_packages_repository_exists "llvm-toolchain"; then
    curl https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
    sudo sh -c "cat << EOF > /etc/apt/sources.list.d/llvm.list
# i386 not available
deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic main
deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic main
# 7
deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-7 main
deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic-7 main
# 8
deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-8 main
deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic-8 main
EOF"
  fi
}

__init_packages_llvm__install() {
  init_packages_depends \
    'clang-9' \
    'clang-format-9' \
    'clang-tidy-9' \
    'clang-tools-9' \
    'libclang-9-dev'
}

__init_packages_llvm__config() {
  sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-9 100
  sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-9 100
  sudo update-alternatives --install /usr/bin/clang-format clang-format /usr/bin/clang-format-9 100
  sudo update-alternatives --install /usr/bin/clang-tidy clang-tidy /usr/bin/clang-tidy-9 100
  sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-9 100
}

## TeX
__init_packages_tex() {
  init_packages_depends \
    'ghostscript' \
    'texlive-full' \
    'xzdec'
}

## Inkscape
__init_packages_inkscape__init() {
  init_packages_add_repository "ppa:inkscape.dev/stable"
}

__init_packages_inkscape__install() {
  init_packages_depends 'inkscape' 'pstoedit' 'wget'
}

__init_packages_inkscape__config() {
  markcd "${HOME}/src"
  wget https://github.com/julienvitard/eqtexsvg/archive/master.tar.gz
  tar xfz master.tar.gz
  chmod +x eqtexsvg-master/eqtexsvg.py
  sudo cp -p eqtexsvg-master/{eqtexsvg.py,eqtexsvg.inx} /usr/share/inkscape/extensions
  getback
}

## Fcitx
__init_packages_fcitx() {
  init_packages_depends 'fcitx-mozc'
}

__init_packages_fcitx__config() {
  mkdir -p "${HOME}/.config/autostart"
  local desktop="/usr/share/fcitx/xdg/autostart/fcitx-autostart.desktop"
  if [[ -f "${desktop}" ]]; then
    cp "${desktop}" "${HOME}/.config/autostart"
  else
    e_warning "Unable to find ${desktop} (${FUNCNAME[0]})"
  fi
}

## Xmodmap
__init_packages_xmodmap() {
  init_packages_always_config
}

__init_packages_xmodmap__config() {
  local file="${HOME}/.config/autostart/xmodmap.desktop"
  if [[ ! -f "${file}" ]]; then
    mkdir -p "${HOME}/.config/autostart"
    cat <<EOF >"${file}"
[Desktop Entry]
Type=Application
Name=xmodmap
Comment=Setup keyboard layout
Exec=xmodmap ${HOME}/.Xmodmap
Terminal=false
Icon=xmodmap
StartupNotify=false
Terminal=true
EOF
  fi
}

## Emacs
__init_packages_emacs__init() {
  init_packages_add_repository "ppa:kelleyk/emacs"
}

__init_packages_emacs__install() {
  init_packages_depends 'cmigemo' 'emacs-mozc-bin' 'emacs26'
}

## Albert
__init_packages_albert__init() {
  if ! init_packages_repository_exists "manuelschneid3r"; then
    curl https://build.opensuse.org/projects/home:manuelschneid3r/public_key \
      | sudo apt-key add -
    sudo sh -c "cat <<EOF >/etc/apt/sources.list.d/home:manuelschneid3r.list
deb http://download.opensuse.org/repositories/home:/manuelschneid3r/xUbuntu_18.04/ /
EOF"
  fi
}

__init_packages_albert__install() {
  init_packages_depends 'albert'
}

init_packages "$@"

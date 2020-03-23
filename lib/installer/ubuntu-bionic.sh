#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO' ERR
set -eEu

THIS_DIR="${1}"; shift
source "${THIS_DIR}/lib/functions.sh"
source "${THIS_DIR}/lib/install_packages.sh"

## Build tools
__install_packages_build() {
  install_packages_depends \
    'automake' \
    'autotools-dev' \
    'build-essential' \
    'cmake'
}

## VCS
__install_packages_vcs() {
  install_packages_depends \
    'git' \
    'mercurial'
}

## Fonts
__install_packages_font() {
  install_packages_depends \
    'fontforge' \
    'fonts-ipafont' \
    'powerline' \
    'ttf-mscorefonts-installer'
}

## Programming language
__install_packages_prog() {
  install_packages_depends \
    'gfortran' \
    'python3-dev' \
    'python3-venv' \
    'python-dev' \
    'python-virtualenv'
}

## Multimedia
__install_packages_multimedia() {
  install_packages_depends \
    'ffmpeg' \
    'imagemagick' \
    'libavcodec-extra' \
    'ubuntu-restricted-extras' \
    'vlc'
}

## Dependencies for milib
# https://www.mi.ams.eng.osaka-u.ac.jp/open-e.html
__install_packages_milib() {
  install_packages_depends \
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
__install_packages_util() {
  install_packages_depends \
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
__install_packages_tmux() {
  install_packages_depends \
    'acpi' \
    'libevent-dev' \
    'libncurses5-dev' \
    'libncursesw5-dev' \
    'libutempter-dev' \
    'sysstat' \
    'wget' \
    'xclip' \
    'xsel'
  install_packages_always_config
}

__install_packages_tmux__config() {
  # Remove tmux if it is installed with apt-get.
  if install_packages_is_installed "tmux"; then
    sudo apt remove -y tmux
  fi

  # Install the latest version of tmux
  local tag=3.0
  if ! has tmux; then
    markcd "${HOME}/src"
    curl -s https://api.github.com/repos/tmux/tmux/releases/tags/$tag \
      | grep "browser_download_url.*\/tmux-.*\.tar\.gz" \
      | cut -d ":" -f 2,3 \
      | tr -d \" \
      | wget -qi -
    local tarball="$(find . -name tmux-*.tar.gz)"
    tar xfz "${tarball}"
    cd "${tarball%.tar.gz}"
    ./configure --prefix=$HOME/usr --enable-utempter
    make && make install
    cd ..
    rm "${tarball}"
    getback
  fi

  # Install or update tmux plugin manager
  git_clone_or_update https://github.com/tmux-plugins/tpm ${HOME}/.tmux/plugins/tpm

  # Install bash-completion for tmux.
  if [[ ! -f ${HOME}/.local/share/bash-completion/completions/tmux ]]; then
    curl -Lo ${HOME}/.local/share/bash-completion/completions/tmux --create-dirs \
         https://raw.githubusercontent.com/imomaliev/tmux-bash-completion/master/completions/tmux
  fi

  # Install tmux plugins.
  tmux new -d -s __noop >/dev/null 2>&1 || true
  tmux set-environment -g TMUX_PLUGIN_MANAGER_PATH "~/.tmux/plugins"
  "$HOME"/.tmux/plugins/tpm/bin/install_plugins || true
  tmux kill-session -t __noop >/dev/null 2>&1 || true
}

## fish
__install_packages_fish__init() {
  install_packages_add_repository 'ppa:fish-shell/release-3'
}

__install_packages_fish__install() {
  install_packages_depends \
    'curl' \
    'fish' \
    'fontforge' \
    'git' \
    'unzip' \
    'wget'
}

__install_packages_fish__config() {
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
  sudo fc-cache -f
  getback

  install_packages_set_msg "$(cat <<EOF
Don't forget execute the following command on fish shell.
  $ fish_update_completions
EOF
)"
}

## Python
__install_packages_python() {
  install_packages_depends \
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
  install_packages_always_config
}

__install_packages_python__config() {
  git_clone_or_update https://github.com/pyenv/pyenv.git ${HOME}/.pyenv
}

## Go
__install_packages_go() {
  install_packages_depends 'wget'
  install_packages_always_config
}

__install_packages_go__config() {
  # Install go
  local latest_ver=1.13.8
  local ver=
  if has go; then
    ver=$(go version | cut -d " " -f 3 | sed s/go//)
  fi
  if ! has go || [[ $ver != $latest_ver ]]; then
    markcd "$HOME/usr/lib"
    local tarball="go${latest_ver}.linux-amd64.tar.gz"
    wget -q https://dl.google.com/go/${tarball}
    rm -rf go
    tar xfz ${tarball}
    rm -f ${tarball}
    getback

    install_packages_set_msg "$(cat <<EOF
Check if appropriate values are set in your shell resource.
  GOPATH=\\\$HOME/.go
  GOROOT=\\\$HOME/usr/lib/go
  PATH=\\\$HOME/usr/lib/go/bin:\\\$PATH
EOF
)"
  fi

  # Install ghq
  GOPATH="$HOME/.go"
  GOROOT="$HOME/usr/lib/go"
  PATH="$HOME/usr/lib/go/bin:$PATH"
  if ! has ghq; then
    go get github.com/motemen/ghq
  fi
}

## fzf -- an interactive filter for command-line.
# https://github.com/junegunn/fzf
__install_packages_fzf() {
  install_packages_always_config
}

__install_packages_fzf__config() {
  git_clone_or_update https://github.com/junegunn/fzf.git ${HOME}/.fzf
  yes | head -n 3 | ~/.fzf/install --no-fish
}

## Node.js
# https://github.com/nodesource/distributions
__install_packages_nodejs__init() {
  # To install bash-language-server we need node.js ver.11, see:
  # https://github.com/mads-hartmann/bash-language-server/issues/131
  local ver=11
  if ! install_packages_repository_exists "nodesource"; then
    curl -sL https://deb.nodesource.com/setup_${ver}.x | sudo -E bash -
  fi
}

__install_packages_nodejs__install() {
  install_packages_depends 'nodejs'
}

__install_packages_nodejs__config() {
  install_packages_set_msg "$(cat <<EOF
To install bash-language-server, execute the following:
  sudo npm install --unsafe-perm -g bash-language-server

For details, see:
https://github.com/mads-hartmann/bash-language-server#installation
EOF
)"
}

## LLVM
__install_packages_llvm__init() {
  if ! install_packages_repository_exists "llvm-toolchain"; then
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

__install_packages_llvm__install() {
  install_packages_depends \
    'clang-9' \
    'clang-format-9' \
    'clang-tidy-9' \
    'clang-tools-9' \
    'libclang-9-dev'
}

__install_packages_llvm__config() {
  sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-9 100
  sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-9 100
  sudo update-alternatives --install /usr/bin/clang-format clang-format /usr/bin/clang-format-9 100
  sudo update-alternatives --install /usr/bin/clang-tidy clang-tidy /usr/bin/clang-tidy-9 100
  sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-9 100
}

## TeX
__install_packages_tex() {
  local space
  space="$(df --output=avail -k /usr | sed '1d;s/[^0-9]//g')"
  # Check if the current file system has enough space (>6.0 GiB).
  if [[ $space > 6291456 ]]; then
    install_packages_depends \
      'ghostscript' \
      'texlive-full' \
      'xzdec'
  else
    e_warning "Skip installation of texlive due to lack of free disk space."
  fi
}

## TexLab - An implementation of the language server protocol for LaTeX.
# https://github.com/latex-lsp/texlab
__install_packages_texlab__install() {
  install_packages_depends 'wget'
  install_packages_always_config
}

__install_packages_texlab__config() {
  local latest_ver ver
  latest_ver=1.1.0
  ver=
  if has texlab; then
    ver=$(texlab --version | cut -d ' ' -f 2)
  fi
  if ! has texlab || [[ $ver != $latest_ver ]]; then
    markcd "$HOME/usr/bin"
    local tarball=texlab-x86_64-linux.tar.gz
    curl -s https://api.github.com/repos/latex-lsp/texlab/releases/latest \
      | grep "browser_download_url.*\/$tarball" \
      | cut -d ":" -f 2,3 \
      | tr -d \" \
      | wget -qi -
    tar xfz "$tarball"
    rm "$tarball"
    getback
  fi
}

## Inkscape
__install_packages_inkscape__init() {
  install_packages_add_repository "ppa:inkscape.dev/stable"
}

__install_packages_inkscape__install() {
  install_packages_depends 'inkscape' 'pstoedit' 'wget'
}

__install_packages_inkscape__config() {
  markcd "${HOME}/src"
  wget -q https://github.com/julienvitard/eqtexsvg/archive/master.tar.gz
  tar xfz master.tar.gz
  chmod +x eqtexsvg-master/eqtexsvg.py
  sudo cp -p eqtexsvg-master/{eqtexsvg.py,eqtexsvg.inx} /usr/share/inkscape/extensions
  getback
}

## GIMP
__install_packages_gimp__init() {
  install_packages_add_repository "ppa:otto-kesselgulasch/gimp"
}

__install_packages_gimp__install() {
  install_packages_depends 'gimp'
}

## Japanese environment
__install_packages_ja__init() {
  if ! install_packages_repository_exists "archive.ubuntulinux.jp"; then
    curl -s https://www.ubuntulinux.jp/ubuntu-ja-archive-keyring.gpg | sudo apt-key add -
    curl -s https://www.ubuntulinux.jp/ubuntu-jp-ppa-keyring.gpg | sudo apt-key add -
    sudo curl -s -Lo /etc/apt/sources.list.d/ubuntu-ja.list https://www.ubuntulinux.jp/sources.list.d/bionic.list
  fi
}

__install_packages_ja__install() {
  install_packages_depends 'ubuntu-defaults-ja'
}

## Fcitx
__install_packages_fcitx() {
  install_packages_depends 'fcitx-mozc'
}

__install_packages_fcitx__config() {
  mkdir -p "${HOME}/.config/autostart"
  local desktop="/usr/share/fcitx/xdg/autostart/fcitx-autostart.desktop"
  if [[ -f "${desktop}" ]]; then
    cp "${desktop}" "${HOME}/.config/autostart"
  else
    e_warning "Unable to find ${desktop} (${FUNCNAME[0]})"
  fi
}

## Sarasa gothic
__install_packages_sarasa() {
  install_packages_depends 'fontforge' 'p7zip-full' 'wget'
  install_packages_always_config
}

__install_packages_sarasa__config() {
  if ! (fc-list | grep -qi sarasa); then
    markcd "${HOME}/.local/share/fonts"
    curl -s https://api.github.com/repos/be5invis/Sarasa-Gothic/releases/latest \
      | grep "browser_download_url.*\/sarasa-gothic-ttf-.*\.7z" \
      | cut -d ":" -f 2,3 \
      | tr -d \" \
      | wget -qi -
    local file="$(find . -name "sarasa-gothic-ttf-*.7z")"
    7z x -y "${file}"
    rm "${file}"
    sudo fc-cache -f
    getback
  fi
}

## Xmodmap
__install_packages_xmodmap() {
  install_packages_always_config
}

__install_packages_xmodmap__config() {
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
__install_packages_emacs__init() {
  install_packages_add_repository "ppa:kelleyk/emacs"
}

__install_packages_emacs__install() {
  install_packages_depends 'cmigemo' 'emacs-mozc-bin' 'emacs26'
}

## watchexec
# watchexec makes Emacs boot faster when using straight.el
__install_packages_watchexec() {
  install_packages_depends 'curl' 'wget' 'python3-venv'
  install_packages_always_config
}

__install_packages_watchexec__config() {
  if ! has watchexec; then
    markcd "${HOME}/src"
    curl -s https://api.github.com/repos/watchexec/watchexec/releases/latest \
      | grep "browser_download_url.*\.deb" \
      | cut -d ":" -f 2,3 \
      | tr -d \" \
      | wget -qi -
    local deb="$(find . -name "watchexec*.deb")"
    sudo apt install "${deb}"
    rm "${deb}"
    getback
  fi
}

## ripgrep
# https://github.com/BurntSushi/ripgrep
__install_packages_rg() {
  install_packages_depends 'curl' 'wget'
  install_packages_always_config
}

__install_packages_rg__config() {
  if ! has rg; then
    markcd "${HOME}/src"
    curl -s https://api.github.com/repos/BurntSushi/ripgrep/releases/latest \
      | grep "browser_download_url.*\.deb" \
      | cut -d ":" -f 2,3 \
      | tr -d \" \
      | wget -qi -
    local deb="$(find . -name "ripgrep*.deb")"
    sudo apt install "${deb}"
    rm "${deb}"
    getback
  fi
}

## bat -- an alternative to cat(1) with syntax highlighting
# https://github.com/sharkdp/bat
__install_packages_bat() {
  install_packages_depends 'curl' 'wget'
  install_packages_always_config
}

__install_packages_bat__config() {
  if ! has bat; then
    markcd "${HOME}/src"
    curl -s https://api.github.com/repos/sharkdp/bat/releases/latest \
      | grep "browser_download_url.*bat_.*_amd64\.deb" \
      | cut -d ":" -f 2,3 \
      | tr -d \" \
      | wget -qi -
    local deb="$(find . -name "bat_*_amd64.deb")"
    sudo dpkg -i "${deb}"
    rm "${deb}"
    getback
  fi
}

## Albert
__install_packages_albert__init() {
  if ! install_packages_repository_exists "manuelschneid3r"; then
    curl https://build.opensuse.org/projects/home:manuelschneid3r/public_key \
      | sudo apt-key add -
    sudo sh -c "cat <<EOF >/etc/apt/sources.list.d/home:manuelschneid3r.list
deb http://download.opensuse.org/repositories/home:/manuelschneid3r/xUbuntu_18.04/ /
EOF"
  fi
}

__install_packages_albert__install() {
  install_packages_depends 'albert'
}

## Docker
__install_packages_docker__init() {
  if ! install_packages_repository_exists "docker"; then
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
    sudo add-apt-repository \
         "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
  fi
}

__install_packages_docker__install() {
  install_packages_depends \
    'docker-ce' \
    'docker-ce-cli' \
    'containerd.io'
}

__install_packages_docker__config() {
  # Create a group named 'docker' and add the user to it.
  if grep -q docker /etc/group; then
    e_warning "Group 'docker' already exists. ($FUNCNAME[0])"
  else
    sudo groupadd docker
  fi
  sudo usermod -aG docker $(whoami)

  # Install bash-completion for docker.
  if [[ ! -f $HOME/.local/share/bash-completion/completions/docker ]]; then
    curl -Lo $HOME/.local/share/bash-completion/completions/docker --create-dirs \
         https://raw.githubusercontent.com/docker/docker-ce/master/components/cli/contrib/completion/bash/docker
  fi

  # Install fish completion for docker.
  if [[ ! -f $HOME/.config/fish/completions/docker.fish ]]; then
    curl -Lo $HOME/.config/fish/completions/docker.fish --create-dirs \
         https://raw.githubusercontent.com/docker/docker-ce/master/components/cli/contrib/completion/fish/docker.fish
  fi
}

install_packages "$@"

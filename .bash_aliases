# My personal aliases.

has() {
  type "$1" >/dev/null 2>&1
}

if has xdg-open; then
  alias open="xdg-open"
fi

if has ipython; then
  alias pylab="ipython --pylab"
fi

if has xsel; then
  alias pbcopy="tr -d '\n' | xsel --clipboard --input"
fi

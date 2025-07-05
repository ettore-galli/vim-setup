# 🧠 Vim + Python Dev Cheat Sheet

Una guida rapida per padroneggiare Vim come ambiente di sviluppo Python.

---

## ⚙️ Comandi Base di Vim

### 🔄 Modalità
- `i` → modalità inserimento  
- `ESC` → modalità normale  
- `:` → modalità comando

### 📁 Gestione File, Buffer e Finestre
- `:e file.py` → apri un file
- `:ls` → lista buffer aperti
- `:bN` → passa al buffer N
- `:bd` → chiudi buffer attuale
- `:split`, `:vsplit` → split orizzontale / verticale
- `Ctrl-w h/j/k/l` → naviga tra finestre
- `:tabnew` → apri nuovo tab

### 📋 Copia/Incolla
- `yy`, `dd`, `p`, `P` → copia, taglia, incolla
- `"+yy`, `"+p` → usa la clipboard di sistema

---

## 🧩 Plugin e Utilizzo Quotidiano

| Plugin | Scopo | Comandi Principali |
|--------|-------|---------------------|
| `vim-plug` | Plugin manager | `:PlugInstall`, `:PlugUpdate`, `:PlugClean` |
| `coc.nvim` | Autocompletamento con Pyright | `:CocInfo`, `gd`, `gr`, `K`, `:CocCommand` |
| `ale` | Linter e formatter | Linting automatico, `:ALEFix`, `:ALELint`, `:ALEInfo` |
| `vim-airline` | Barra di stato & tabline | Configurazione automatica, minimal setup |
| `nerdtree` | File explorer | `Ctrl+n`, `o` (apri), `t` (nuovo tab), `q` (chiudi) |
| `vim-python/python-syntax` | Sintassi Python avanzata | Attivo by default |
| `vim-fugitive` | Integrazione Git | `:Git`, `:Gstatus`, `:Gdiffsplit`, `:Gblame`, `:Glog` |

---

## 💡 Tip & Tricks

- Salva file: `:w`  
- Esci: `:q`, o entrambi: `:wq`
- Undo/Redo: `u` / `Ctrl-r`  
- Ripeti ultimo comando: `.`
- Sostituzione globale: `:%s/da/a/g`

---

> 🧪 Linting: ale + flake8 | 🛠️ Formatter: autopep8 (fix all save)
>  
> ✨ Autocomplete & IntelliSense: coc.nvim + coc-pyright

---

## 📂 Percorso Plugin

Tutti i plugin sono installati in: `~/.vim/plugged`

---

Made with ❤️ & :coffee: in Vim


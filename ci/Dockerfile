FROM ubuntu

# ---------- BASE ----------

RUN apt-get update && \  
    apt-get install -y \
    git \
    sudo \
    curl \
    build-essential \
    python3 \
    python3.10-venv \
    libncurses-dev \
    python3-pip 

RUN curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
RUN sudo apt install -y nodejs

ENV HOME=/var/vimdev

RUN groupadd -g 501 vimdev    
RUN useradd -m \
    -d $HOME \
    -s /usr/bin/bash \
    -g vimdev \
    -G sudo \
    -u 501 \
    vimdev

RUN echo "vimdev:password" | chpasswd

USER vimdev:sudo

WORKDIR $HOME 

# ---------- VIM ----------

RUN mkdir -p $HOME/vim-ide

WORKDIR $HOME/vim-ide 

RUN git clone https://github.com/vim/vim.git

RUN cd vim && \
    ./configure --prefix=$HOME/vim-ide/vim-install \
            --with-features=huge \
            --enable-python3interp \
            --enable-terminal \
            --enable-cscope \
            --enable-multibyte \
            --enable-fail-if-missing && \
        make -j$(nproc) && \
        make install && \
        curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
         https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim


RUN echo 'export PATH=$PATH:$HOME/vim-ide/vim-install/bin' >> $HOME/.bashrc
ENV PATH=$PATH:$HOME/vim-ide/vim-install/bin 

COPY ci/bin/.vimrc $HOME/.vimrc
RUN vim +'PlugInstall --sync' +qa

# ---------- PYTHON ----------

RUN python3 -m venv .venv && \
    . .venv/bin/activate  && \
    pip install --upgrade pip && \
    pip install pipx && \
    pipx ensurepath && \
    pipx install ruff && \
    pipx install mypy && \
    pipx install black && \
    pipx install isort && \
    pipx install pytest

# ---------- START ---------- 

COPY ci/bin/entrypoint $HOME/entrypoint
ENTRYPOINT ["/var/vimdev/entrypoint"]

FROM ubuntu

RUN apt-get update && \  
    apt-get install -y \
    git \
    sudo \
    curl \
    build-essential \
    python3 \
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



# ENV VIM_PACK_BASE=.vim/pack

# ENV VIM_PACK_PLUGINS=$VIM_PACK_BASE/plugins/start

# RUN mkdir -p $VIM_PACK_BASE \
#     && mkdir -p $VIM_PACK_PLUGINS

# RUN git -C $VIM_PACK_PLUGINS clone https://github.com/preservim/nerdtree \
# && git -C $VIM_PACK_PLUGINS clone https://github.com/itchyny/vim-gitbranch.git \
# && git -C $VIM_PACK_PLUGINS clone https://github.com/itchyny/lightline.vim \
# && git -C $VIM_PACK_PLUGINS clone https://github.com/dracula/vim.git 

COPY ci/bin/entrypoint $HOME/entrypoint
# COPY ci/bin/.vimrc $HOME/.vimrc

ENTRYPOINT ["/var/vimdev/entrypoint"]

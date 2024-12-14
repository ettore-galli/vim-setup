FROM ubuntu

RUN apt-get update && \  
    apt-get install -y \
    git \
    vim \
    sudo \
    curl \
    python3 \
    python3-pip

RUN groupadd -g 501 vimdev    
RUN useradd -m \
    -d /var/vimdev \
    -s /usr/bin/bash \
    -g vimdev \
    -G sudo \
    -u 501 \
    vimdev

RUN echo "vimdev:password" | chpasswd

USER vimdev:sudo

WORKDIR /var/vimdev

ENV VIM_PACK_BASE=.vim/pack

ENV VIM_PACK_PLUGINS=$VIM_PACK_BASE/plugins/start

RUN mkdir -p $VIM_PACK_BASE \
    && mkdir -p $VIM_PACK_PLUGINS

RUN git -C $VIM_PACK_PLUGINS clone https://github.com/preservim/nerdtree \
&& git -C $VIM_PACK_PLUGINS clone https://github.com/itchyny/vim-gitbranch.git \
&& git -C $VIM_PACK_PLUGINS clone https://github.com/itchyny/lightline.vim \
&& git -C $VIM_PACK_PLUGINS clone https://github.com/dracula/vim.git 

COPY ci/bin/entrypoint /var/vimdev/entrypoint
COPY ci/bin/.vimrc     /var/vimdev/.vimrc

ENTRYPOINT ["/var/vimdev/entrypoint"]

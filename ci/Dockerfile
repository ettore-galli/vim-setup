FROM ubuntu

ENV USER=vimdev

RUN apt-get update && \  
    apt-get install -y \
    git \
    vim \
    sudo \
    curl \
    python3 \
    python3-pip

RUN groupadd -g 1001 ${USER}    
RUN useradd -m \
    -d /var/${USER} \
    -s /usr/bin/bash \
    -g ${USER} \
    -G sudo \
    -u 1001 \
    ${USER}

RUN echo "${USER}:password" | chpasswd

USER ${USER}:sudo

WORKDIR /var/${USER}

ENV VIM_PACK_BASE=.vim/pack

ENV VIM_PACK_PLUGINS=$VIM_PACK_BASE/plugins/start

RUN mkdir -p $VIM_PACK_BASE \
    && mkdir -p $VIM_PACK_PLUGINS

RUN git -C $VIM_PACK_PLUGINS clone https://github.com/preservim/nerdtree \
&& git -C $VIM_PACK_PLUGINS clone https://github.com/vim-airline/vim-airline \
&& git -C $VIM_PACK_PLUGINS clone https://github.com/vim-airline/vim-airline-themes \
&& git -C $VIM_PACK_PLUGINS clone https://github.com/dracula/vim.git \
&& git -C $VIM_PACK_PLUGINS clone https://github.com/christianrickert/vim-firefly.git 

COPY ci/bin/entrypoint /var/${USER}/entrypoint
COPY ci/bin/.vimrc     /var/${USER}/.vimrc

ENTRYPOINT ["sh", "-c", "/var/${USER}/entrypoint"]

FROM ubuntu


RUN groupadd -g 1001 vimuser

RUN apt-get update && \  
    apt-get install -y \
    git \
    vim \
    sudo \
    curl \
    python3 \
    python3-pip


RUN useradd -m \
    -d /var/vimuser \
    -s /usr/bin/bash \
    -g vimuser \
    -G sudo \
    -u 1001 \
    vimuser


RUN echo "vimuser:password" | chpasswd

USER vimuser:sudo

WORKDIR /var/vimuser

ENV VIM_PACK_BASE=.vim/pack

ENV VIM_PACK_COLORSCHEMES=$VIM_PACK_BASE/colorschemes/start
ENV VIM_PACK_SYNTAX=$VIM_PACK_BASE/syntax/start
ENV VIM_PACK_OTHERS=$VIM_PACK_BASE/others/start

RUN mkdir -p $VIM_PACK_BASE \
    && mkdir -p $VIM_PACK_COLORSCHEMES \
    && mkdir -p $VIM_PACK_SYNTAX \
    && mkdir -p $VIM_PACK_OTHERS

RUN git -C $VIM_PACK_OTHERS clone https://github.com/preservim/nerdtree \
&& git -C $VIM_PACK_OTHERS clone https://github.com/vim-airline/vim-airline \
&& git -C $VIM_PACK_COLORSCHEMES clone https://github.com/vim-airline/vim-airline-themes \
&& git -C $VIM_PACK_COLORSCHEMES clone https://github.com/dracula/dracula-theme 

COPY ci/bin/entrypoint /var/vimuser/entrypoint
COPY ci/bin/.vimrc     /var/vimuser/.vimrc

ENTRYPOINT ["/var/vimuser/entrypoint"]

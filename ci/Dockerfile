FROM ubuntu


RUN groupadd -g 1001 vimuser

RUN apt-get update && \  
    apt-get install -y git vim sudo

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

RUN git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

COPY ci/bin/entrypoint /var/vimuser/entrypoint
COPY ci/bin/.vimrc     /var/vimuser/.vimrc

RUN vim +PluginInstall +qall




ENTRYPOINT ["/var/vimuser/entrypoint"]

FROM ubuntu


RUN groupadd -g 1001 vimuser

RUN apt-get update && \  
    apt-get install -y \
    git \
    vim \
    sudo \
    curl \
    python3


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

RUN mkdir -p ~/.config
RUN cp -r .vim/bundle/powerline/powerline/config_files ~/.config/powerline


ENTRYPOINT ["/var/vimuser/entrypoint"]

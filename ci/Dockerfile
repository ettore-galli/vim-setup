FROM ubuntu


RUN groupadd -g 1001 vimuser

RUN useradd -m \
    -d /var/vimuser \
    -s /usr/bin/bash \
    -g vimuser \
    -u 1001 \
    vimuser
 

RUN apt-get update  

RUN apt-get install -y git 


USER vimuser:vimuser

WORKDIR ~

RUN git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

COPY ci/bin/entrypoint /var/vimuser/entrypoint

ENTRYPOINT ["/var/vimuser/entrypoint"]

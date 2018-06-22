FROM ocaml/opam

RUN eval `opam config env` && opam update && opam depext conf-m4.1 && opam install jbuilder core alcotest menhir ansiterminal lambda-term ocamlgraph
RUN sudo apt update && sudo apt-get install lua5.3-dev make

USER opam
ADD --chown=opam:nogroup jbuild ./jbuild
RUN mkdir lib && mkdir test && mkdir test/config
ADD --chown=opam:nogroup lib/*.ml? lib/*.ml lib/*.c ./lib/
ADD --chown=opam:nogroup lib/jbuild ./lib/jbuild
ADD --chown=opam:nogroup lib/config/*.ml ./lib/config/
ADD --chown=opam:nogroup lib/config/jbuild ./lib/config/jbuild
ADD --chown=opam:nogroup test/*.ml ./test/
ADD --chown=opam:nogroup test/jbuild ./test/jbuild
ADD --chown=opam:nogroup bin/*.ml ./bin/
ADD --chown=opam:nogroup bin/jbuild ./bin/jbuild
ADD --chown=opam:nogroup stdlib/* ./stdlib/
ADD luml.opam ./luml.opam
ADD jbuild ./jbuild
ADD Makefile ./Makefile
ENV PATH="/home/opam/.opam/4.05.0/bin:${PATH}"
RUN make install

CMD ["/bin/sh", "--login", "c", "make install"]

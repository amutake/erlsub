# Default rule
default:
	jbuilder build @install

test:
	jbuilder runtest

install:
	jbuilder install ${NAME}

uninstall:
	jbuilder uninstall ${NAME}

reinstall: uninstall install

clean:
	rm -rf _build
	rm ${NAME}.install

.PHONY: default test install uninstall reinstall clean

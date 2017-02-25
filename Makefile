NAME := erlsub

# Default rule
default:
	jbuilder build-package $(NAME)

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

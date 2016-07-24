.PHONY: all distclean clean info compile doc install

REBAR3_URL = https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR),)
REBAR = $(CURDIR)/rebar3
endif

#
# Check for required packages
#
REQUIRED_PKGS := \
	erl \
	dialyzer

_ := $(foreach pkg,$(REQUIRED_PACKAGES),\
		$(if $(shell which $(pkg)),\
			$(error Missing required package $(pkg)),))

ERLANG_VER=$(shell erl -noinput -eval 'io:put_chars(erlang:system_info(system_version)),halt().')

QDHSERV := _build/default/bin/qdhserv

all: compile

$(QDHSERV): src/*
	$(REBAR) do clean, escriptize

clean: $(REBAR)
	$(REBAR) clean --all

distclean:
	@rm -rf _build

compile: $(QDHSERV)

info: $(REBAR)
	@echo 'Erlang/OTP system version: $(ERLANG_VER)'
	@$(REBAR) --version

doc: $(QDHSERV)
	$(REBAR) edoc

install: $(QDHSERV)
	install -d $(DESTDIR)/usr/local/bin
	install -m755 $< $(DESTDIR)/usr/local/bin

$(REBAR):
	curl -s -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x $(REBAR)

# ex: ts=4 sts=4 sw=4 noet

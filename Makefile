REBAR?=./rebar

all: build

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean

distclean: clean
	@rm -rf deps

build: depends
	$(REBAR) compile

depends:
	@if test ! -d ./deps; t@hen \
		$(REBAR) get-deps; \
	else \
		$(REBAR) update-deps; \
	fi


.PHONY: doc

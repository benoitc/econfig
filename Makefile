REBAR?=./rebar

all: build

clean:
	$(REBAR) clean

build: depends
	$(REBAR) compile

depends:
	@if test ! -d ./deps; then \
		$(REBAR) get-deps; \
	else \
		$(REBAR) update-deps; \
	fi

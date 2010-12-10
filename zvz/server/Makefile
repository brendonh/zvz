PKG_NAME = zvz
NODE_NAME = zvz_dev@dev.brendonh.org

SRC_DIR = src
EBIN_DIR = ebin
INCLUDE_DIR = include

LIB_DIR = /home/brendonh/lib

MOCHI_BASE = $(LIB_DIR)/mochiweb
AMF_BASE = $(LIB_DIR)/erlang-amf
SQLITE_BASE = $(LIB_DIR)/sqlite-erlang

SOURCES  = $(wildcard $(SRC_DIR)/*.erl)
INCLUDES = $(wildcard $(INCLUDE_DIR)/*.hrl)
PARSERS = $(wildcart $(PRIV_DIR)/*.yrl)

TARGETS  = $(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

ERLC_INCLUDES = -I $(INCLUDE_DIR) -I $(PG_BASE)/include -I $(AMF_BASE)/include
ERL_EBINS = -pa $(EBIN_DIR) -pa $(SQLITE_BASE)/ebin -pa $(AMF_BASE)/ebin -pa $(MOCHI_BASE)/ebin

ERLC = erlc
ERLC_OPTS = $(ERLC_INCLUDES) -o $(EBIN_DIR) -Wall +debug_info

ERL_CMD=PATH=$(SQLITE_BASE)/priv:$$PATH erl \
    -name $(NODE_NAME) \
	-boot start_sasl \
	-config $(PKG_NAME) \
	+W w \
	$(ERL_EBINS)

all: $(TARGETS)

run_prereqs: all

run: run_prereqs
	$(ERL_CMD) -s zvz_app launch

daemon: run_prereqs
	$(ERL_CMD) -detached -s zvz_app launch

test: run_prereqs
	$(ERL_CMD) -noshell -s zvz_avatars test -s init stop

stop:
	erl_call -a '$(PKG_NAME)_app stop_and_halt []' -sname $(PKG_NAME)

clean: cleanlog
	rm -f $(TARGETS)
	rm -f $(EBIN_DIR)/*.beam

cleanlog:
	rm -f auth.log report.log sasl.log
	rm -f *.access

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_OPTS) $<

cbin/babylon.so: force_look
	@echo Checking csrc
	cd csrc; $(MAKE) $(MFLAGS)

force_look :
	@true

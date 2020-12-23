CC ?= gcc
LDFLAGS ?= -lm 
CFLAGS ?= -Wall --std=c99
CFLAGS_DEBUG := -g3 \
    -O \
    -DDEBUG
CFLAGS_RELEASE := -O2 \
    -march=native \
    -mtune=native \
    -ftree-vectorize

SRC_DIR := src
OBJ_DIR := obj
BIN_DIR := bin
INC_DIR := includes
EXE := carm

SRC := $(wildcard $(SRC_DIR)/*.c)
OBJ := $(SRC:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

all: debug
debug: CFLAGS += $(CFLAGS_DEBUG)
debug: $(BIN_DIR)/$(EXE)
release: CFLAGS += $(CFLAGS_RELEASE)
release: $(BIN_DIR)/$(EXE)

$(BIN_DIR)/$(EXE): $(OBJ) | $(BIN_DIR)
	$(CC) $^ -o $@ $(LDFLAGS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) -c $< $(CFLAGS) -o $@ $(LDFLAGS) -I $(INC_DIR)

$(OBJ_DIR) $(BIN_DIR):
	mkdir -p $@

clean:
	@$(RM) -rv $(BIN_DIR) $(OBJ_DIR)

.PHONY: clean all debug release

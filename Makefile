.PHONY: dirs all clean cl

GHC = ghc
GHCFLAGS = --make
CLASSDIR = obj
SRCDIR = src
EXECUTABLE = clisp

.SUFFIXES: .hs .o

$(CLASSDIR)/%.o : $(SRCDIR)/%.hs
	@$(GHC) $(GHCFLAGS) -isrc -outputdir $(CLASSDIR) -o $(EXECUTABLE) $<
	@echo "Created executable $(EXECUTABLE)"

all: dirs classestocompile

SOURCEFILES := \
				Main.hs \
				Monad.hs \
				Types.hs \
				Parser.hs \
				Evaluator.hs \

classestocompile: $(addprefix $(CLASSDIR)/, $(SOURCEFILES:.hs=.o))

run: all
	@echo "Running executable $(EXECUTABLE); type 'quit' to exit."
	@./$(EXECUTABLE)

dirs:
	@mkdir -p $(CLASSDIR)

cl: clean

clean:
	@rm -rf $(CLASSDIR)
	@rm -f $(EXECUTABLE)

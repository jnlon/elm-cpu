# Make sure to set SRC in your envirnment to the file you are compiling! eg
# $ SRC=thing.elm make
BROWSER=firefox
ELMC=elm-make
OUTDIR=html
TARGET=$(OUTDIR)/$(SRC).html

all: $(TARGET) ;

$(TARGET): $(SRC) $(OUTDIR) $(LIB)
	$(ELMC) $(SRC) --output $(TARGET)

$(OUTDIR):
	mkdir $(OUTDIR)

run: $(TARGET)
	$(BROWSER) $(TARGET)

clean:
	rm -r $(OUTDIR)

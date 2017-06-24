

site: site.hs
	stack build

build: site
	stack exec site build

rebuild: site
	stack exec site rebuild

watch: site
	stack exec site watch

publish: build
	{ \
	git add --ignore-errors papers.bib sections/* stack.yaml talks/* static/* templates/* site.hs Setup.hs index.tex LICENSE Makefile ;\
	git commit -m "Updated content" ;\
	git push origin source ;\
	}

	{ \
	cp -r _site/* _build/ ;\
	cp LICENSE _build/ ;\
	cd _build ;\
	git add --ignore-errors index.html papers/* static/* talks/* LICENSE ;\
	git commit -m "Compiled page" ;\
	git push origin master ;\
	}

sync: .git
	{ \
	git pull origin source ;\
	}

	{ \
	cd _build ;\
	git pull origin master ;\
	}

clean: site
	stack exec site clean

clean-all: clean
	stack clean

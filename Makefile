.PHONY: dev
dev:
	elm-live src/Main.elm --output=./site/main.js --warn --yes --debug --dir=./site

.PHONY: build
build: format build-without-formatting

.PHONY: build-without-formatting
build-without-formatting:
	elm-make src/Main.elm --output=./site/main.js --yes --warn --debug
	cp ./elm-stuff/exact-dependencies.json ./

.PHONY: format
format:
	elm-format ./src --yes

.PHONY: deploy
deploy:
	rsync \
		--delete \
		--archive \
		--rsh ssh \
		--verbose \
		--checksum \
		--exclude='.git/' \
		./site/ \
		root@45.33.68.74:/root/depths/site

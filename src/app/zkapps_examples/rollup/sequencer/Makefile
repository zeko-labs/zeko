build:
	DUNE_PROFILE=devnet dune build

docker:
	git status --porcelain --ignored | grep '^[!][!] ' | cut -d' ' -f2 > Dockerfile.dockerignore
	cd ../../../../.. ; \
	docker build -t zeko -f src/app/zkapps_examples/rollup/sequencer/Dockerfile .

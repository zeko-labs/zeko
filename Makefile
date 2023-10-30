sequencer-docker:
	cp -R $(shell readlink -f sequencer/node_modules/o1js) o1js
	docker build -t zeko-sequencer -f ./sequencer.Dockerfile .
	rm -rf o1js

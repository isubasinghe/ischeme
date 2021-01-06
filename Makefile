all:
	stack build

wasm:
	rm -rf ./ischeme-wasm/src
	cp -r src ./ischeme-wasm/src
	cd ischeme-wasm && ahc-cabal new-install --installdir . --overwrite-policy=always && ahc-dist --input-exe ischeme-wasm --export-function=handleReq --no-main --input-mjs register.mjs --bundle --browser


wasm-clean: 
	rm -rf ./ischeme-wasm/src
	rm -rf ./ischeme-wasm/*.*js
	rm -rf ./ischeme-wasm/*.wasm
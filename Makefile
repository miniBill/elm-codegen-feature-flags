PORT ?= 8000

preview-docs:
	npx elm-doc-preview --port "$(PORT)"

examples-elm-codegen-install:
	cd examples && \
		npx elm-codegen install

generate-example:
	cd examples && \
		npx elm-codegen run --output generated

preview-generated-docs: generate-example
	cd examples && \
		npx elm-doc-preview --port "$(PORT)"

examples-reactor: generate-example
	cd examples && \
		elm reactor --port "$(PORT)"

test-example: generate-example
	cd examples && \
		npx elm-test

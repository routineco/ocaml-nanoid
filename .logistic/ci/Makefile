MAKEFILE=$(abspath $(lastword $(MAKEFILE_LIST)))
DIR=$(patsubst %/,%,$(dir $(MAKEFILE)))
OCAML_VERSION ?= $(shell ocamlc --version)

build: lint
	dune subst
	dune build

lint:
	dune build @fmt

check:
	dune subst
	dune runtest

package:
	dune subst
	dune install --prefix=_install
	if ! test -e Dockerfile; then echo "FROM alpine" > Dockerfile; fi
	sed '1aADD _install/ /usr/' Dockerfile > Dockerfile.package
	docker build -f "Dockerfile.package" . -t "$(IMAGE):$$(git describe --always)"
	docker push "$(IMAGE):$$(git describe --always)"
	docker tag "$(IMAGE):$$(git describe --always)" "$(IMAGE):$$SLUG"
	docker push "$(IMAGE):$$SLUG"
	echo "docker-image \"$(IMAGE):$$(git describe --always)\"" > metrics.txt

package/clean:
	docker rmi "$(IMAGE):$$(git describe --always)" "$(IMAGE):$$SLUG"

ci/image:
	cp "$(CI_ID_RSA)" .logistic/id_rsa
	cp "$(CI_ID_RSA_PUB)" .logistic/id_rsa.pub
	if test -e Dockerfile.ci; then cat Dockerfile.ci >> "$(dir $(MAKEFILE_LIST))Dockerfile.ci"; fi
	if test -z "$$FROM_SCRATCH"; then \
		if test -n "$(IMAGE_CI)$(VARIANT)" && docker pull "$(IMAGE_CI)$(VARIANT)"; then \
			echo "using remote $(CI_COMMIT_REF_NAME) cache";  \
	                CACHE='--cache-from "$(IMAGE_CI)$(VARIANT)"' ; \
		elif test -n "$(IMAGE_CI_MASTER)" && docker pull "$(IMAGE_CI_MASTER)"; then \
			echo "using remote master cache"; \
			CACHE='--cache-from "$(IMAGE_CI_MASTER)"'; \
		else \
			echo "using local cache"; \
			CACHE=''; \
	        fi; \
	else \
		echo "building from scratch"; \
		CACHE='--no-cache'; \
	fi; \
	docker build -f "$(dir $(MAKEFILE_LIST))Dockerfile.ci" . --tag "$(or $(IMAGE_CI)$(VARIANT),ci)" --build-arg OCAML_VERSION=$(OCAML_VERSION) --build-arg PACKAGES="$(PACKAGES)" --build-arg OPAM_PACKAGES="$(OPAM_PACKAGES)" --build-arg AWS_SECRET_ACCESS_KEY="$(AWS_SECRET_ACCESS_KEY)" $$CACHE

image/%:
	if test -e Dockerfile.$*; then cat Dockerfile.$* >> "$(dir $(MAKEFILE_LIST))Dockerfile.$*"; fi
	if test -n "$(FROM_SCRATCH)"; then CACHE="--no-cache"; \
	else if docker pull "$(IMAGE)/$*"; then CACHE="--cache-from $(IMAGE)/$*"; fi; \
	fi; \
	docker build -f "$(dir $(MAKEFILE_LIST))Dockerfile.$*" . --tag "$(IMAGE)/$*" $$CACHE

opam:
	$(DIR)/opam-publish

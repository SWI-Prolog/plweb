VOLUME=$(shell cd .. && pwd)/data
PORT=3400

PUBLISH=--publish=${PORT}:3400 --publish=3420:2022
DOPTS=--rm ${PUBLISH} -v ${VOLUME}:/srv/plweb/data
IMG=plweb

all:
	@echo "Targets"
	@echo
	@echo "image            Build the plweb image"
	@echo "run              Run the image (detached)"
	@echo "restart          Stop and restart the image"
	@echo
	@echo "update-mostly	Rebuild most except for the OS"
	@echo "update-swipl	Rebuild swipl and plweb"
	@echo "update-plweb	Rebuild plweb"

image::
	docker build -t $(IMG) .

run:
	docker run --name=plweb --detach ${DOPTS} $(IMG)

restart:
	docker stop plweb
	make run

bash:
	docker run -it ${DOPTS} $(IMG) --bash

update-plweb:
	sed -i "s/PLWEB_VERSION.*/PLWEB_VERSION $$(date)/" Dockerfile
	make image

update-swipl:
	sed -i "s/SWIPL_VERSION.*/SWIPL_VERSION $$(date)/" Dockerfile
	make image

update-mostly:
	sed -i "s/REBUILD_MOST.*/REBUILD_MOST $$(date)/" Dockerfile
	make image


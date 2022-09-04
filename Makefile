up:
	@echo "Starting dev environment"
	@docker compose \
		-f ./dockerfiles/docker-compose.yml \
		up -d

down:
	@echo "Stopping dev environment"
	@docker compose \
		-f ./dockerfiles/docker-compose.yml \
		down --remove-orphans

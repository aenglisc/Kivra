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

generate: \
	generate_sender_server \
	generate_sender_client

generate_%_server:
	@echo "Generating $* API server"
	@docker run \
		--rm \
		-v ${PWD}:/local \
			openapitools/openapi-generator-cli generate \
				-i /local/specs/$*.yml \
				-g erlang-server \
				-o /local/$*/apps/openapi_$*_server \
				--additional-properties packageName=openapi_$*_server

generate_%_client:
	@echo "Generating $* API client"
	@docker run \
		--rm \
		-v ${PWD}:/local \
			openapitools/openapi-generator-cli generate \
				-i /local/specs/$*.yml \
				-g erlang-client \
				-o /local/$*/apps/openapi_$*_client \
				--additional-properties packageName=openapi_$*_client

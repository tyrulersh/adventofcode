all:
	@echo Targets:
	@echo   docker - creates the docker image for developing advent solutions

docker:
	docker build -t haskell-dev-arch-linux .

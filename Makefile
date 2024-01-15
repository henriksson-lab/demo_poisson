docker:
	docker build -t demo_poisson .
docker_testrun:
	docker run --rm -p 3838:3838 demo_poisson

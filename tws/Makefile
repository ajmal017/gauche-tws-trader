

hist:
	docker run -ti --network=host -v $(shell pwd):/code -w /code tws python3 /code/history.py

run:
	docker run -ti --network=host -v $(shell pwd):/code -w /code tws python3 /code/order.py

sample:
	docker run -ti --network=host tws python3 /work/IBJts/samples/Python/Testbed/Program.py

cancel:
	docker run -ti --network=host tws python3 /work/IBJts/samples/Python/Testbed/Program.py -C

FROM python:latest

COPY twsapi_macunix.976.01.zip /work/

WORKDIR /work
RUN unzip twsapi_macunix.976.01.zip

WORKDIR /work/IBJts/source/pythonclient
RUN python3 setup.py sdist
RUN python3 setup.py bdist_wheel
RUN python3 -m pip install --user --upgrade dist/ibapi-9.76.1-py3-none-any.whl

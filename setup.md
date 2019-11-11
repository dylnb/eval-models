First run:

```
docker run -p 8888:8888 -v $PWD:/home/jovyan/pwd --env JUPYTER_ENABLE_LAB=yes i--env JUPYTER_TOKEN=x --name ihaskell_notebook crosscompass/ihaskell-notebook
```

Subsequent runs:

```
docker start -i ihaskell_notebook
```


On start or restart, in jupyterlab terminal:

```
cd pwd
stack build
```
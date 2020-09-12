## Eval Models

Various Rational Speech Act models, mostly aimed at interpreting vague utterances

For the simulations reported in Bumford and Rett 2020 _Rationalizing Evaluativity_
(SuB 25), see [`src/sub25/eval-basic.ipynb`](src/sub25/eval-basic.ipynb)



### Notes to self

First run in new environment:

`docker run -p 8888:8888 -v $PWD:/home/jovyan/pwd --env JUPYTER_ENABLE_LAB=yes --env JUPYTER_TOKEN=x --name eval-notebook crosscompass/ihaskell-notebook:latest`

Subsequent runs when reconnecting:

`docker start -i eval-notebook` (if necessary, `sudo service docker start`)

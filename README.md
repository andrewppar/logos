# Welcome to λogos

λogos is a proof assisstant designed to be used by philosophers. Once a theorem is entered to be proved λogos keeps track of the current goal and the premises that are relevant to showing that goal. This means that for complex or exploratory proofs there's much less cognitive overhead. 

Because λogos is aimed at philosophers the focus was on developing a very expressive language. As such the language the λogos understands is a higher-order logic, allowing for quantification over any position in a formula and formulas that have other formulas as arguments. In this way it's possible to express that Alice believes everything the Hatter says

<img align="right" style="width: 49%;" src="public/hatter-alone.jpg">
	
```
(forall ?x 
  (implies 
   (!says Hatter ?x)
   (!believes Alice ?x)))
```

The above formula may look a little strange. I encourage any new users to start up λogos and read through the Tutorial tab to get an idea of how things work. The Formulas tab provides a more formal explanation of the language and decisions made for the system.

## Quick Run 

### Script 

If all of the [prerequisites](prerequisites) are installed on your system λogos can be launched from this directory using the launcher. From a terminal that's in the same directory as this README call

```
./run_logos.sh
```

then just visit [localhost:3000](http://localhost:3000) in your favorite browser. 

### Docker Image

If you have [Docker Desktop](https://www.docker.com/get-started/) installed then there is no need to install any other prerequisites, Docker handles that for you. Go to the directory that this README is in in a terminal and build a docker image with 

```
docker build -t logos .
```
The above command will have to download all the dependencies for the project so it could take a few minutes to complete.
Once it's done, run that image with 

```
docker run --rm -it -p 3000:3000 -p 4000:4000 -p 9630:9630 logos
```

When your terminal says 
```
[:client] Configuring build.
[:client] Compiling ...
[:client] Build completed. (243 files, 242 compiled, 0 warnings, 29.79s)
```
or something similar visit [localhost:3000](http://localhost:3000) in your favorite browser. 

NOTE: The docker image is not the most secure way to launch the app, so I don't recommend using it in a production setting.


## Prerequisites

All of the following commands are intended to be performed in the terminal. How you access the terminal may differ from machine to machine. On a mac, you can type Command and Space (CMD+SPC) to open spotlight -- from there type terminal and hit enter. A new terminal window should open up.

1. Install `npm`. Directions for this depending on your platform can be found [here](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
2. Install npm dependencies: 
   1. shadow-cljs: `npm i shadow-cljs`
   2. bulma - A CSS framework: `npm i bulma`
   3. bulma tooltip - An extension to the standard bulma: `npm i bulma-tooltip`
   
3. Install java (openjdk). You can check whether java is already installed by calling `java --version`. If it is not installed directions for your platform can be found [1](https://openjdk.java.net/install/ "here") 
   
4. Install leiningen. Directions can be found here. (https://leiningen.org/#install "here")

## Development 

In order to develop for λogos, you'll need to have all the [prerequisites](prerequisites) installed. Then you can use your preferred method of editing clojure and clojurescript to develop against a running server. 

### Running

To start a web server for the application, navigate to this directory and run in a terminal window:
```
lein run
```	

In a separate terminal window navigate to this directory and run 
```
npm run watch
```

## License

Source Copyright © 2022 Andrew Parisi. Distributed under the Eclipse Public License, the same as Clojure uses. See the file COPYING.

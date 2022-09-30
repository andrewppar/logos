FROM clojure:lein-2.9.8-alpine

RUN apk add --update nodejs npm
RUN npm i shadow-cljs
RUN npm i bulma
RUN npm i bulma-tooltip

COPY shadow-cljs.edn shadow-cljs.edn
COPY public/ public/
COPY src/ src/
COPY node_modules/ node_modules/
COPY package.json package.json
COPY project.clj project.clj

ENV LOGOS_SERVER localhost

RUN npx shadow-cljs npm-deps
RUN lein deps


EXPOSE 3000
CMD lein run & npm run watch
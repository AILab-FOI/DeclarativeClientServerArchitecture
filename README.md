# DeclarativeClientServerArchitecture
Diplomski rad: Implementacija arhitekture klijent-poslužitelj korištenjem deklarativnog programskog jezika

# Pokretanje projekata
## Mnesia i server
- potrebno je instalirati [erlang](https://www.erlang.org/) verziju OTP 27 te [rebar3](https://rebar3.org/) posljednju verziju 
- pozicioniranje u projekt server ```cd server/``` te pokretanje aplikacije komandom ```rebar3 shell```
- rest servis dostupan je na putanji http://localhost:5000/

## Rust klijent
- potrebno je instalirati cargo, što je najjednostavnije putem [rustup](https://rustup.rs/) instalacijskog paketa
- pozicioniranje u projekt client ```cd client/```
- pokrenuti komandu ```wasm-pack build --target web```
- klijent aplikacija je sada spremna za korištenje u Angular

## Angular web aplikacija
- potrebno je instalirati [node](https://nodejs.org/en)
- pozicioniranje u projekt webapp ```cd webapp/```
- prije pokretanja potrebno je osigurati da se kompajlirana verzija rust klijenta nalazi unutar Angular projekta
- kopirati direktortij ```client/pkg``` u Angular projekt na putanju ```webapp/src/assets/```
- ili kreirati slabu vezu tako da se pozicionira u direktorij ```webapp/src/assets``` te izvrši komanda ```ln -s ../../../client/pkg pkg```
- pozicionirati se u direktortij ```webapp/```, instalirati pakete pomoću komande ```npm install``` te pokrenuti aplikaciju komandom ```npm run start```
- web aplikacija dostupna je na putanji http://localhost:4200

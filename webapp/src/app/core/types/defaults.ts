import { Fakultet, Korisnik } from './elevated_types';

export const defaultKorisnik: () => Korisnik = () => ({
  id: 0,
  ime: '',
  prezime: '',
  opis: '',
  oib: 0,
  email: '',
  dodatno: {},
  uloga: '',
  kolegiji: [],
});

export const defaultFakultet: () => Fakultet = () => ({
  id: 0,
  naziv: '',
  adresa: {},
  korisnici: [],
  katedre: [],
});

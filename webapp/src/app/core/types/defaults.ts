import {
  Fakultet,
  Katedra,
  Kolegij,
  Sadrzaj,
  Sekcija,
  Korisnik,
} from './elevated_types';

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

export const defaultSekcija: () => Sekcija = () => ({
  id: 0,
  naziv: '',
  opis: '',
  sadrzaj: [],
  vidljivo: true,
});

export const defaultKatedra: () => Katedra = () => ({
  id: 0,
  naziv: '',
  opis: '',
  kolegiji: [],
});

export const defaultKolegij: () => Kolegij = () => ({
  id: 0,
  naziv: '',
  skraceno: '',
  slika: '',
  sekcije: [],
  studenti: [],
});

export const defaultLekcija: () => Sadrzaj = () => ({
  id: 0,
  naziv: '',
  tip: 'lekcija',
  vrijednost: {},
});

export const defaultDokument: () => Sadrzaj = () => ({
  id: 0,
  naziv: '',
  tip: 'dokument',
  vrijednost: {},
});
export const defaultPoveznica: () => Sadrzaj = () => ({
  id: 0,
  naziv: '',
  tip: 'poveznica',
  vrijednost: {},
});

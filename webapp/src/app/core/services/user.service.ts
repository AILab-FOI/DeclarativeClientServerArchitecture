import { Injectable, signal } from '@angular/core';
import { dohvati_korisnika } from '../../../assets/pkg/client';
import { jwtDecode } from 'jwt-decode';
import { Korisnik } from '../types/elevated_types';

const defaultKorisnik: () => Korisnik = () => ({
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

@Injectable({
  providedIn: 'root',
})
export class UserService {
  public user = signal<Korisnik>(defaultKorisnik());

  constructor() {
    this.dohvati_korisnika();
  }

  public dohvati_korisnika() {
    if (localStorage.getItem('AT')) {
      let token = localStorage.getItem('AT');
      let id = parseInt(jwtDecode(token).sub);
      dohvati_korisnika(id, token).then((res: Korisnik) => {
        this.user.set(res);
      });
    }
  }
}

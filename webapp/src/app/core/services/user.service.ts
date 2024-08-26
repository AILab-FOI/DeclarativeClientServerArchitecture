import { computed, inject, Injectable, signal } from '@angular/core';
import { dohvati_korisnika } from '../../../assets/pkg/client';
import { jwtDecode } from 'jwt-decode';
import { Korisnik } from '../types/elevated_types';
import { TokenService } from './token.service';

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
  public readonly isWorker = computed(() => {
    return this.user().uloga === 'Djelatnik';
  });
  public readonly routePrefix = computed(() => {
    if (this.isWorker()) return 'worker/';
    return 'student/';
  });

  private token = inject(TokenService);

  constructor() {}

  public dohvati_korisnika() {
    if (this.token.exists()) {
      let token = this.token.accessToken();
      let id = parseInt(jwtDecode(token).sub);
      return dohvati_korisnika(id, token);
    }
    return new Promise((resolve) => {
      resolve(defaultKorisnik());
    });
  }
}

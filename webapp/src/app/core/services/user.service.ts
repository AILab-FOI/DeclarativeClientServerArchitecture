import { effect, inject, Injectable, signal } from '@angular/core';
import { dohvati_korisnika, Student } from '../../../assets/pkg/client';
import { Response, WasmService } from './wasm.service';
import { jwtDecode } from 'jwt-decode';
import { AuthService } from './auth.service';

export type Korisnik = {
  id: number;
  ime: string;
  prezime: string;
  opis: string;
};

const defaultKorisnik: () => Korisnik = () => ({
  id: 0,
  ime: '',
  prezime: '',
  opis: '',
});

@Injectable({
  providedIn: 'root',
})
export class UserService {
  public user = signal<Korisnik>(defaultKorisnik());
  constructor() {}

  public dohvatiKorisnika() {
    if (localStorage.getItem('AT')) {
      let token = localStorage.getItem('AT');
      let id = parseInt(jwtDecode(token).sub);
      dohvati_korisnika(id, token).then((res: Response<Student>) => {
        console.log(res);
        this.user.set({ ...res.data });
      });
    }
  }
}

import { computed, inject, Injectable, signal } from '@angular/core';
import { dohvati_korisnika } from '../../../assets/pkg/client';
import { jwtDecode } from 'jwt-decode';
import { Korisnik } from '../types/elevated_types';
import { TokenService } from './token.service';
import { defaultKorisnik } from '../types/defaults';

@Injectable({
  providedIn: 'root',
})
export class UserService {
  public user = signal<Korisnik>(defaultKorisnik());
  public readonly isWorker = computed(() => {
    return this.user().uloga !== 'Student';
  });

  public readonly isDean = computed(() => {
    return this.user().uloga === 'Dekan';
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

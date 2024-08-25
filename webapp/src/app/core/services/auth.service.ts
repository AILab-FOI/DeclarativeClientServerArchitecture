import { inject, Injectable, signal } from '@angular/core';
import { Router } from '@angular/router';
import { Response, WasmService } from './wasm.service';
import { Error, login, Tokens } from '../../../assets/pkg/client';
import { jwtDecode } from 'jwt-decode';
import { UserService } from './user.service';

export type LoginHandler<T> = { status: boolean; data: T };

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  private router = inject(Router);
  private wasm = inject(WasmService);
  private user = inject(UserService);
  constructor() {}

  async login(email: string, password: string): Promise<LoginHandler<string>> {
    return login(email, password)
      .then((e: Response<Tokens>) => {
        localStorage.setItem('AT', e.data.access_token);
        localStorage.setItem('RT', e.data.refresh_token);
        this.user.dohvati_korisnika();
        return {
          status: true,
          data: '',
        };
      })
      .catch((err: Error) => ({ status: false, data: err.message }));
  }

  logout(): void {
    localStorage.removeItem('AT');
    localStorage.removeItem('RT');
    this.router.navigate(['home']);
  }

  isAuthenticated(): boolean {
    return localStorage.getItem('AT') !== null;
  }
}

import { inject, Injectable, signal } from '@angular/core';
import { Router } from '@angular/router';
import { Response } from './wasm.service';
import { MyError, login, Tokens } from '../../../assets/pkg/client';
import { UserService } from './user.service';
import { TokenService } from './token.service';
import { defaultKorisnik } from '../types/defaults';

export type LoginHandler<T> = { status: boolean; data: T };

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  private router = inject(Router);
  private token = inject(TokenService);
  private user = inject(UserService);

  async login(email: string, password: string): Promise<LoginHandler<string>> {
    return login(email, password)
      .then(async (e: Response<Tokens>) => {
        this.token.setAccessToken(e.data.access_token);
        return this.user.dohvati_korisnika().then((res) => {
          this.user.user.set(res);
          return {
            status: true,
            data: '',
          };
        });
      })
      .catch((err: MyError) => ({ status: false, data: err.message }));
  }

  logout(): void {
    this.token.removeAccessToken();
    this.user.user.set(defaultKorisnik());
    this.router.navigate(['home']);
  }

  isAuthenticated(): boolean {
    return this.token.exists();
  }
}

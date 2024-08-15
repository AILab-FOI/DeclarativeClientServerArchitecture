import { inject, Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { Response, WasmService } from './wasm.service';
import { login } from '../../../assets/pkg/client';

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  private router = inject(Router);
  private wasm = inject(WasmService);
  constructor() {}

  async login(email: string, password: string): Promise<boolean> {
    let response: Response<string> = await this.wasm.callWasmFunction(
      login(email, password),
    );
    if (response.data) {
      localStorage.setItem('token', response.data);
      return true;
    }
    return false;
  }

  logout(): void {
    localStorage.removeItem('token');
    this.router.navigate(['home']);
  }

  isAuthenticated(): boolean {
    return localStorage.getItem('token') !== null;
  }
}

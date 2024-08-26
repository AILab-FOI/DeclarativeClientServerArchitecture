import { computed, Injectable, signal } from '@angular/core';

@Injectable({
  providedIn: 'root',
})
export class TokenService {
  public setAccessToken(token: string): void {
    localStorage.setItem('access_token', token);
  }

  public removeAccessToken(): void {
    localStorage.removeItem('access_token');
  }

  public accessToken(): string {
    return localStorage.getItem('access_token');
  }

  public exists(): boolean {
    return localStorage.getItem('access_token') !== null;
  }
}

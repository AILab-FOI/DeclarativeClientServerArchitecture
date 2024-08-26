import { inject, Injectable } from '@angular/core';
import { dohvati_fakultete, Fakultet } from '../../../assets/pkg/client';
import { Response } from './wasm.service';
import { TokenService } from './token.service';

@Injectable({
  providedIn: 'root',
})
export class FacultyService {
  private token = inject(TokenService);
  constructor() {}

  public dohvati_fakultete() {
    if (this.token.exists()) {
      let token = this.token.accessToken();
      dohvati_fakultete(token).then((res: Response<Fakultet>) => {
        console.log(res);
      });
    }
  }
}

import { Injectable } from '@angular/core';
import { dohvati_fakultete, Fakultet } from '../../../assets/pkg/client';
import { Response } from './wasm.service';

@Injectable({
  providedIn: 'root',
})
export class FacultyService {
  constructor() {}

  public dohvati_fakultete() {
    if (localStorage.getItem('AT')) {
      let token = localStorage.getItem('AT');
      dohvati_fakultete(token).then((res: Response<Fakultet>) => {
        console.log(res);
      });
    }
  }
}

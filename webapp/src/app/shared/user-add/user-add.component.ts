import {
  Component,
  effect,
  inject,
  input,
  model,
  output,
  signal,
} from '@angular/core';
import { Korisnik, TokenService } from '../../core';
import { UserCardComponent } from '../user-card/user-card.component';
import { dohvati_korisnike_na_fakultetu } from '../../../assets/pkg/client';
import { NgOptimizedImage } from '@angular/common';

export type UserAddMode = 'all' | 'workers';

@Component({
  selector: 'app-user-add',
  standalone: true,
  imports: [UserCardComponent, NgOptimizedImage],
  templateUrl: './user-add.component.html',
  styleUrl: './user-add.component.scss',
})
export class UserAddComponent {
  data = model.required<Korisnik>();
  token = inject(TokenService);
  mode = input.required<UserAddMode>();
  fakultet = input.required<number>();
  sudionici = input.required<Korisnik[]>();
  korisnici = signal<Korisnik[]>([]);
  query = output<Korisnik>();
  selected = signal<number>(0);
  constructor() {
    effect(() => {
      dohvati_korisnike_na_fakultetu(
        this.fakultet(),
        this.token.accessToken(),
      ).then((res: Korisnik[]) => {
        this.korisnici.set(
          res.filter((k) => {
            if (this.mode() === 'all') {
              return this.sudionici().find((s) => s.id === k.id) === undefined;
            } else {
              return (
                k.uloga !== 'Student' &&
                this.sudionici().find((s) => s.id === k.id) === undefined
              );
            }
          }),
        );
        if (this.mode() === 'workers') {
          this.korisnici.update((k) => {
            let nm = k.map((o) => {
              o.tip = 'voditelj';
              return o;
            });
            return nm;
          });
        }
      });
    });
  }

  editTip(value: string): void {
    this.data.update((k) => {
      k.tip = value;
      return k;
    });
    this.query.emit(this.data());
  }

  select(korisnik: Korisnik) {
    this.data.set(korisnik);
    this.selected.set(korisnik.id);
    this.query.emit(this.data());
  }
}

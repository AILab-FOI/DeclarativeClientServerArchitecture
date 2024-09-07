import { Component, effect, inject, input, model, signal } from '@angular/core';
import {
  defaultKorisnik,
  Kolegij,
  Korisnik,
  TokenService,
  UserService,
} from '../../core';
import { ButtonComponent } from '../button/button.component';
import { Router } from '@angular/router';
import { openModal } from '../modal/modal.component';
import { Dialog } from '@angular/cdk/dialog';
import { UserAddComponent } from '../user-add/user-add.component';
import {
  dodaj_djelatnika_na_kolegij,
  dodaj_studenta_na_kolegij,
  dohvati_kolegij,
  obrisi_djelatnika_na_kolegiju,
  obrisi_studenta_na_kolegiju,
} from '../../../assets/pkg/client';
import { NgOptimizedImage } from '@angular/common';

@Component({
  selector: 'app-participants',
  standalone: true,
  imports: [ButtonComponent, NgOptimizedImage],
  templateUrl: './participants.component.html',
  styleUrl: './participants.component.scss',
})
export class ParticipantsComponent {
  public course = input.required<number>();
  public userService = inject(UserService);
  public tokenService = inject(TokenService);
  private router = inject(Router);
  private dialog = inject(Dialog);
  public participants = signal<Korisnik[]>([]);

  goTo(id: number): void {
    this.router.navigate(['profile', id]);
  }

  constructor() {
    effect(() => {
      dohvati_kolegij(this.course(), this.tokenService.accessToken()).then(
        (res: Kolegij) => {
          this.participants.set(res.studenti.concat(res.djelatnici));
        },
      );
    });
  }

  openEdit(): void {
    let ref = openModal<Korisnik>(this.dialog, {
      data: defaultKorisnik(),
      tool: { view: UserAddComponent },
      inputs: {
        fakultet: this.userService.user().fakultet.id,
        sudionici: this.participants(),
        mode: 'all',
      },
      title: 'Edit section',
    });
    ref.componentInstance['query'].subscribe((user: Korisnik) => {
      if (user && user.id !== 0) {
        if (user.uloga === 'Student') {
          dodaj_studenta_na_kolegij(
            this.course(),
            user.id,
            this.tokenService.accessToken(),
          ).then((res) => {
            if (res) {
              dohvati_kolegij(
                this.course(),
                this.tokenService.accessToken(),
              ).then((res: Kolegij) => {
                this.participants.set(res.djelatnici.concat(res.studenti));
              });
            }
          });
        } else {
          dodaj_djelatnika_na_kolegij(
            this.course(),
            user.id,
            'Nositelj',
            this.tokenService.accessToken(),
          ).then((res) => {
            if (res) {
              dohvati_kolegij(
                this.course(),
                this.tokenService.accessToken(),
              ).then((res: Kolegij) => {
                this.participants.set(res.djelatnici.concat(res.studenti));
              });
            }
          });
        }
      }
      ref.close();
    });
  }
  delete(user: Korisnik): void {
    if (user.uloga === 'Student') {
      obrisi_studenta_na_kolegiju(
        this.course(),
        user.id,
        this.tokenService.accessToken(),
      ).then((_) => {
        dohvati_kolegij(this.course(), this.tokenService.accessToken()).then(
          (res: Kolegij) => {
            this.participants.set(res.djelatnici.concat(res.studenti));
          },
        );
      });
    } else {
      obrisi_djelatnika_na_kolegiju(
        this.course(),
        user.id,
        this.tokenService.accessToken(),
      ).then((_) => {
        dohvati_kolegij(this.course(), this.tokenService.accessToken()).then(
          (res: Kolegij) => {
            this.participants.set(res.djelatnici.concat(res.studenti));
          },
        );
      });
    }
  }
}

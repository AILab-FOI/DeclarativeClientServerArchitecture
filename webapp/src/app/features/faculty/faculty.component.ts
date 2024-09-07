import {
  Component,
  computed,
  effect,
  inject,
  input,
  signal,
} from '@angular/core';
import {
  defaultKatedra,
  Fakultet,
  Katedra,
  TokenService,
  UserService,
} from '../../core';
import {
  dodaj_djelatnika_na_katedru,
  dodaj_katedru,
  dodaj_katedru_na_fakultet,
  dohvati_fakultet,
  obrisi_katedru,
  uredi_fakultet,
} from '../../../assets/pkg/client';
import { DepartmentCardComponent } from '../../shared/department-card/department-card.component';

import { CommonModule, NgOptimizedImage } from '@angular/common';
import { MapComponent } from '../../shared/map/map.component';
import { openModal } from '../../shared';
import { Dialog } from '@angular/cdk/dialog';
import { FacultyEditComponent } from '../../shared/faculty-edit/faculty-edit.component';
import { DepartmentAddComponent } from '../../shared/department-add/department-add.component';

@Component({
  selector: 'app-faculty',
  standalone: true,
  imports: [
    CommonModule,
    DepartmentCardComponent,
    MapComponent,
    NgOptimizedImage,
  ],
  templateUrl: './faculty.component.html',
  styleUrl: './faculty.component.scss',
})
export class FacultyComponent {
  public tokenService = inject(TokenService);
  public userService = inject(UserService);
  public id = input.required<string>();
  public faculty = signal<Fakultet>(undefined);
  private dialog = inject(Dialog);
  public userOnFaculty = computed(() => {
    return this.userService.user().fakultet.id === this.faculty()?.id;
  });
  constructor() {
    effect(() => {
      dohvati_fakultet(parseInt(this.id())).then((fakultet) => {
        this.faculty.set(fakultet);
      });
    });
  }
  openEdit(): void {
    let ref = openModal<Fakultet>(this.dialog, {
      data: structuredClone(this.faculty()),
      tool: { view: FacultyEditComponent },
      inputs: {},
      title: 'Edit faculty',
    });
    ref.componentInstance['query'].subscribe((faculty: Fakultet) => {
      if (faculty) {
        uredi_fakultet(
          faculty.id,
          faculty.naziv,
          faculty.opis,
          faculty.logo,
          this.tokenService.accessToken(),
        ).then((res) => {
          dohvati_fakultet(res).then((fakultet) => {
            this.faculty.set(fakultet);
            this.userService.dohvati_korisnika().then((res) => {
              this.userService.user.set(res);
            });
          });
        });
      }
      ref.close();
    });
  }

  openEditDepartment(): void {
    let ref = openModal<Katedra>(this.dialog, {
      data: structuredClone(defaultKatedra()),
      tool: { view: DepartmentAddComponent },
      inputs: {},
      title: 'Add department',
    });
    ref.componentInstance['query'].subscribe((department: Katedra) => {
      if (department) {
        dodaj_katedru(
          department.naziv,
          department.opis,
          this.tokenService.accessToken(),
        ).then((dep) => {
          dodaj_katedru_na_fakultet(
            dep,
            this.faculty().id,
            this.tokenService.accessToken(),
          ).then((res) => {
            if (res) {
              dodaj_djelatnika_na_katedru(
                dep,
                this.userService.user().id,
                'voditelj',
                this.tokenService.accessToken(),
              ).then((res) => {
                if (res) {
                  dohvati_fakultet(parseInt(this.id())).then((fakultet) => {
                    this.faculty.set(fakultet);
                  });
                }
              });
            }
          });
        });
      }
      ref.close();
    });
  }

  delete(value: number): void {
    obrisi_katedru(value, this.tokenService.accessToken()).then((_) => {
      dohvati_fakultet(parseInt(this.id())).then((fakultet) => {
        this.faculty.set(fakultet);
      });
    });
  }
}

import {
  Component,
  computed,
  effect,
  inject,
  input,
  signal,
} from '@angular/core';
import {
  defaultKolegij,
  Katedra,
  Kolegij,
  Korisnik,
  TokenService,
  UserService,
} from '../../core';
import {
  dodaj_djelatnika_na_katedru,
  dodaj_djelatnika_na_kolegij,
  dodaj_kolegij,
  dodaj_kolegij_na_katedru,
  dohvati_katedru,
  obrisi_djelatnika_na_katedri,
  obrisi_kolegij,
  uredi_katedru,
} from '../../../assets/pkg/client';
import { Dialog } from '@angular/cdk/dialog';
import {
  CourseCardComponent,
  openModal,
  UserCardComponent,
} from '../../shared';
import { DepartmentEditComponent } from '../../shared/department-edit/department-edit.component';
import { CourseAddComponent } from '../../shared/course-add/course-add.component';
import { UserAddComponent } from '../../shared/user-add/user-add.component';
import { ButtonComponent } from '../../shared/button/button.component';

@Component({
  selector: 'app-department',
  standalone: true,
  imports: [CourseCardComponent, UserCardComponent, ButtonComponent],
  templateUrl: './department.component.html',
  styleUrl: './department.component.scss',
})
export class DepartmentComponent {
  public tokenService = inject(TokenService);
  public userService = inject(UserService);
  public id = input.required<string>();
  public department = signal<Katedra>(undefined);
  public isUserLeader = computed(() => {
    if (this.tokenService.exists()) {
      if (
        this.userService.user().uloga !== 'Student' &&
        this.department().djelatnici.find(
          (k) => k.id === this.userService.user().id,
        )?.tip === 'voditelj'
      ) {
        return true;
      }
    }
    return false;
  });
  private dialog = inject(Dialog);
  constructor() {
    effect(() => {
      dohvati_katedru(parseInt(this.id())).then((department) => {
        this.department.set(department);
      });
    });
  }

  openEdit(): void {
    let ref = openModal<Katedra>(this.dialog, {
      data: structuredClone(this.department()),
      tool: { view: DepartmentEditComponent },
      inputs: {},
      title: 'Edit department',
    });
    ref.componentInstance['query'].subscribe((department: Katedra) => {
      if (department) {
        uredi_katedru(
          department.id,
          department.naziv,
          department.opis,
          this.tokenService.accessToken(),
        ).then((res) => {
          dohvati_katedru(res).then((department) => {
            this.department.set(department);
          });
        });
      }
      ref.close();
    });
  }

  openEditCourses(): void {
    let ref = openModal<Kolegij>(this.dialog, {
      data: structuredClone(defaultKolegij()),
      tool: { view: CourseAddComponent },
      inputs: {},
      title: 'Add course',
    });
    ref.componentInstance['query'].subscribe((course: Kolegij) => {
      if (course) {
        dodaj_kolegij(
          course.naziv,
          course.skraceno,
          this.tokenService.accessToken(),
        ).then((course) => {
          dodaj_kolegij_na_katedru(
            this.department().id,
            course,
            this.tokenService.accessToken(),
          ).then((_) => {
            dodaj_djelatnika_na_kolegij(
              course,
              this.userService.user().id,
              'Nositelj',
              this.tokenService.accessToken(),
            ).then((res) => {
              if (res) {
                dohvati_katedru(parseInt(this.id())).then((department) => {
                  this.department.set(department);
                });
              }
            });
          });
        });
      }
      ref.close();
    });
  }

  openEditWorkers(): void {
    let ref = openModal<Korisnik[]>(this.dialog, {
      data: structuredClone([]),
      tool: { view: UserAddComponent },
      inputs: {
        fakultet: this.department().fakultet.id,
        sudionici: this.department().djelatnici,
        mode: 'workers',
      },
      title: 'Add worker',
    });
    ref.componentInstance['query'].subscribe((user: Korisnik) => {
      if (user) {
        dodaj_djelatnika_na_katedru(
          this.department().id,
          user.id,
          user.tip,
          this.tokenService.accessToken(),
        ).then((_) => {
          dohvati_katedru(parseInt(this.id())).then((department) => {
            this.department.set(department);
          });
        });
      }
      ref.close();
    });
  }

  deleteCourse(id: number): void {
    obrisi_kolegij(id, this.tokenService.accessToken()).then((_) =>
      dohvati_katedru(parseInt(this.id())).then((department) => {
        this.department.set(department);
      }),
    );
  }
  deleteWorker(id: number): void {
    obrisi_djelatnika_na_katedri(
      parseInt(this.id()),
      id,
      this.tokenService.accessToken(),
    ).then((_) => {
      dohvati_katedru(parseInt(this.id())).then((department) => {
        this.department.set(department);
      });
    });
  }
}

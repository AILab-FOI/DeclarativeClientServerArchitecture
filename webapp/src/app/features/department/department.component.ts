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
  dodaj_djelatnika_na_kolegij,
  dodaj_kolegij,
  dodaj_kolegij_na_katedru,
  dohvati_katedru,
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

@Component({
  selector: 'app-department',
  standalone: true,
  imports: [CourseCardComponent, UserCardComponent],
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
      console.log(this.userService.user().uloga);
      if (
        this.userService.user().uloga !== 'Student' &&
        this.department().djelatnici.find(
          (k) => k.id === this.userService.user().id,
        ).tip === 'voditelj'
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
          console.log(res);
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
          ).then((res) => {
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
      inputs: {},
      title: 'Add course',
    });
    ref.componentInstance['query'].subscribe((user: Korisnik) => {
      if (user) {
        console.log(user);
        // TODO: UREĐIVANJE SEKCIJE QUERY
      }
      ref.close();
    });
  }
}

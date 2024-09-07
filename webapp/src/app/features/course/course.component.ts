import { CommonModule, Location } from '@angular/common';
import {
  Component,
  computed,
  effect,
  inject,
  input,
  signal,
} from '@angular/core';
import {
  defaultSekcija,
  DjelatnikKolegij,
  Kolegij,
  Sekcija,
  StudentKolegij,
  UserService,
} from '../../core';
import {
  dodaj_sekciju,
  dodaj_sekciju_na_kolegij,
  dohvati_djelatnika_na_kolegiju,
  dohvati_studenta_na_kolegiju,
  uredi_kolegij,
} from '../../../assets/pkg/client';
import { TokenService } from '../../core/services/token.service';
import {
  GradesComponent,
  openModal,
  ParticipantsComponent,
  SectionComponent,
} from '../../shared';
import { ToastrService } from 'ngx-toastr';
import { Dialog } from '@angular/cdk/dialog';
import { SectionAddComponent } from '../../shared/section-add/section-add.component';
import { CourseEditComponent } from '../../shared/course-edit/course-edit.component';

@Component({
  selector: 'app-course',
  standalone: true,
  imports: [
    CommonModule,
    SectionComponent,
    ParticipantsComponent,
    GradesComponent,
  ],
  templateUrl: './course.component.html',
  styleUrl: './course.component.scss',
})
export class CourseComponent {
  public user = inject(UserService);
  private token = inject(TokenService);
  private toast = inject(ToastrService);
  public course = computed(() => {
    return this.data()?.kolegij;
  });
  public id = input.required<number>();
  public data = signal<DjelatnikKolegij | StudentKolegij>(undefined);
  public page = signal(0);
  private location = inject(Location);
  private dialog = inject(Dialog);

  constructor() {
    effect(() => {
      this.get(true);
    });
  }
  openEditSection(): void {
    let ref = openModal<Sekcija>(this.dialog, {
      data: structuredClone(defaultSekcija()),
      tool: { view: SectionAddComponent },
      inputs: {},
      title: 'Add section',
    });
    ref.componentInstance['query'].subscribe((section: Sekcija) => {
      if (section) {
        dodaj_sekciju(
          section.naziv,
          section.opis,
          this.token.accessToken(),
        ).then((res) => {
          dodaj_sekciju_na_kolegij(
            res,
            this.course().id,
            this.token.accessToken(),
          ).then((res) => {
            if (res) {
              this.get(true);
            }
          });
        });
      }
      ref.close();
    });
  }

  openEditCourse(): void {
    let ref = openModal<Kolegij>(this.dialog, {
      data: structuredClone(this.course()),
      tool: { view: CourseEditComponent },
      inputs: {},
      title: 'Edit course',
    });
    ref.componentInstance['query'].subscribe((course: Kolegij) => {
      if (course) {
        uredi_kolegij(
          course.id,
          course.naziv,
          course.skraceno,
          course.slika,
          this.token.accessToken(),
        ).then((res) => {
          this.get(true);
        });
      }
      ref.close();
    });
  }

  get(reaction: boolean) {
    if (reaction) {
      if (this.user.isWorker()) {
        dohvati_djelatnika_na_kolegiju(
          this.user.user().id,
          this.id(),
          this.token.accessToken(),
        )
          .then((res) => {
            this.data.set(res as DjelatnikKolegij);
          })
          .catch((err) => {
            this.toast.error(err);
            this.location.back();
          });
      } else {
        dohvati_studenta_na_kolegiju(
          this.user.user().id,
          this.id(),
          this.token.accessToken(),
        )
          .then((res) => {
            this.data.set(res as StudentKolegij);
          })
          .catch((err) => {
            this.toast.error(err);
            this.location.back();
          });
      }
    }
  }
}
